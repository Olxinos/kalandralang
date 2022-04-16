(* USES clap *)
(* USES re *)

open Misc

exception Parse_error of {
    file: string;
    line: int;
    char1: int;
    char2: int;
    message: string
  }

let () =
  Printexc.register_printer @@ function
  | Failure message ->
      Some message
  | Parse_error { file; line; char1; char2; message } ->
      Some (sf "File %S, line %d, characters %d-%d: %s" file line char1 char2 message)
  | _ ->
      None

let parse_recipe lexbuf =
  try
    try
      Parser.program Lexer.token lexbuf
    with
      | Parsing.Parse_error ->
          failwith "parse error"
  with Failure message ->
    let file = lexbuf.lex_start_p.pos_fname in
    let line = lexbuf.lex_start_p.pos_lnum in
    let char1 = lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol in
    let char2 = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_start_p.pos_bol in
    raise (Parse_error { file; line; char1; char2; message })

let parse_recipe_stdin () =
  parse_recipe (Lexing.from_channel stdin)

let parse_recipe_file filename =
  let ch = open_in filename in
  Fun.protect ~finally: (fun () -> close_in ch) @@ fun () ->
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = filename };
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_recipe lexbuf

let parse_recipe filename =
  match filename with
    | None -> parse_recipe_stdin ()
    | Some filename -> parse_recipe_file filename

let run_recipe library recipe ~count ~verbose =
  let debug s = if verbose then print_endline s in
  let module A = Interpreter.Amount in
  let paid = ref A.zero in
  let gained = ref A.zero in
  let worst_loss = ref 0. in
  let best_profit = ref 0. in
  let loss_count = ref 0 in
  let profit_count = ref 0 in
  let show_amount ?(divide_by = 1) amount =
    Cost.show_chaos_amount (A.to_chaos amount /. float divide_by)
  in
  let histogram = Histogram.create () in
  try
    for i = 1 to count do
      if i > 1 then echo "";
      let state = Interpreter.(run library (start ~echo: print_endline ~debug recipe)) in
      paid := A.add !paid state.paid;
      gained := A.add !gained state.gained;
      let profit = A.sub state.gained state.paid |> A.to_chaos in
      if profit >= 0. then
        (
          best_profit := max !best_profit profit;
          incr profit_count;
        )
      else
        (
          worst_loss := min !worst_loss profit;
          incr loss_count;
        );
      Option.iter (fun item -> echo "%s" (Item.show item)) state.item;
      echo "Cost:";
      (
        A.iter state.paid @@ fun currency amount ->
        echo "%6d × %s" amount (AST.show_currency currency)
      );
      if A.is_zero state.gained then
        echo "Total: %s" (show_amount state.paid)
      else
        echo "Total: %s — Profit: %s"
          (show_amount state.paid)
          (show_amount (A.sub state.gained state.paid));
      Histogram.add histogram (A.to_exalt state.paid);
    done;
    if count >= 2 then
      let show_average = show_amount ~divide_by: count in
      echo "";
      echo "Average cost (out of %d):" count;
      (
        A.iter !paid @@ fun currency amount ->
        echo "%9.2f × %s" (float amount /. float count) (AST.show_currency currency)
      );
      if A.is_zero !gained then
        echo "Total: %s" (show_average !paid)
      else
        echo "Total: %s — Profit: %s"
          (show_average !paid)
          (show_average (A.sub !gained !paid));
      echo "";
      Histogram.output histogram ~w: 80 ~h: 12 ~unit: "ex"
  with Interpreter.Failed (state, exn) ->
    Option.iter (fun item -> echo "%s" (Item.show item)) state.item;
    echo "Error: %s" (Printexc.to_string exn)

let cache_filename = "data/kalandralang.cache"

let load_from_json () =
  echo "Loading base_items.json...";
  Base_item.load "data/base_items.json";
  echo "Loading mods.json...";
  Mod.load "data/mods.json";
  echo "Loading stat_translations.json...";
  Stat_translation.load "data/stat_translations.json";
  echo "Loading essences.json...";
  Essence.load "data/essences.json"

let save_to_data () =
  echo "Writing %s to speed up future loadings..." cache_filename;
  Cache.export cache_filename;
  echo "You can delete the JSON files now if you want."

let load_from_data () =
  echo "Loading %s..." cache_filename;
  match Cache.import cache_filename with
    | Failed_to_load ->
        echo "Failed to read cache from: %s" cache_filename;
        false
    | Wrong_version ->
        echo "%s is from a different version and cannot be loaded." cache_filename;
        false
    | Loaded ->
        true

let costs_json_filename = "data/costs.json"

let load () =
  if Sys.file_exists cache_filename then (
    if not (load_from_data ()) then (
      load_from_json ();
      save_to_data ();
    )
  )
  else (
    load_from_json ();
    save_to_data ();
  );
  if Sys.file_exists costs_json_filename then (
    echo "Loading %s..." costs_json_filename;
    Cost.load costs_json_filename;
  )
  else (
    echo "%s does not exist, will use default values." costs_json_filename;
    echo "Use the 'write-default-costs' or 'write-ninja-costs' command to create it.";
  );
  echo "Ready."

let find pattern =
  let rex = rex_glob (String.lowercase_ascii pattern) in
  let matches s = String.lowercase_ascii s =~ rex in
  let find_in_base_item _ (base_item: Base_item.t) =
    if matches (Id.show base_item.name) then (
      Pretext.to_channel stdout (Base_item.pp base_item);
      echo "";
    )
  in
  Id.Map.iter find_in_base_item !Base_item.id_map;
  let find_in_mod (modifier: Mod.t) =
    let stat_matches (stat: Mod.stat) =
      match Id.Map.find_opt stat.id !Stat_translation.by_id with
        | None ->
            false
        | Some translations ->
            let translation_matches (translation: Stat_translation.translation) =
              let show_part (part: Stat_translation.string_part) =
                match part with
                  | Constant s -> s
                  | Stat _ -> "#"
              in
              let string =
                translation.string
                |> List.map show_part
                |> String.concat ""
              in
              matches string
            in
            List.exists translation_matches translations
    in
    if
      matches (Id.show modifier.id) ||
      List.exists stat_matches modifier.stats
    then (
      echo "%s" (Mod.show With_ranges modifier);
      echo "%s" (Pretext.show (Mod.pp modifier));
    )
  in
  List.iter find_in_mod !Mod.pool;
  ()

let main () =
  let version =
    Clap.flag
      ~set_long: "version"
      ~description: "Output version and exit."
      false
  in
  if version then (
    echo "0.1.0";
    exit 0
  );
  let command =
    Clap.subcommand [
      (
        Clap.case "run" ~description: "Run a recipe." @@ fun () ->
        let count =
          Clap.default_int
            ~long: "count"
            ~short: 'c'
            ~description: "How many times to run the recipe."
            1
        in
        let verbose =
          Clap.flag
            ~set_long: "verbose"
            ~set_short: 'v'
            ~description: "Print each operation that is performed."
            false
        in
        let seed =
          Clap.optional_int
            ~long: "seed"
            ~description:
              "Seed for the pseudo-random number generator (PRNG). By \
               default, a seed is chosen randomly by the system. Using \
               this option allows you to reproduce specific runs, for \
               debugging or to show off extremely lucky crafts."
            ()
        in
        let show_seed =
          Clap.flag
            ~set_long: "show-seed"
            ~description:
              "Print the seed used by the pseudo-random number \
               generator (PRNG) seed. Can be used to quickly repeat \
               execution until something weird or interesting happens, \
               to then reproduce it with --seed."
            false
        in
        let filename =
          Clap.optional_string
            ~placeholder: "FILE"
            ~description:
              "Path to the file containing the recipe to run. If \
               unspecified, read the recipe from stdin."
            ()
        in
        `run (filename, count, verbose, seed, show_seed)
      );
      (
        Clap.case "format"
          ~description:
            "Format a recipe to make it look better. This does not \
             change the file, it prints the formatted version on \
             standard output."
        @@ fun () ->
        let filename =
          Clap.optional_string
            ~placeholder: "FILE"
            ~description:
              "Path to the file containing the recipe to format. If \
               unspecified, read the recipe from stdin."
            ()
        in
        `format filename
      );
      (
        Clap.case "compile"
          ~description:
            "Compile a recipe and print the compiled version on \
             stdout. Use this for debugging the internal compilation \
             mechanism."
        @@ fun () ->
        let filename =
          Clap.optional_string
            ~placeholder: "FILE"
            ~description:
              "Path to the file containing the recipe to compile. If \
               unspecified, read the recipe from stdin."
            ()
        in
        `compile filename
      );
      (
        Clap.case "find"
          ~description:
            "Find the identifier of a base item from its name, or \
             modifier from its English translation."
        @@ fun () ->
        let pattern =
          Clap.mandatory_string
            ~placeholder: "PATTERN"
            ~description:
              "Pattern to look for, e.g. 'agate' or 'to skill gem \
               level'. You can use shell-like glob expressions, \
               e.g. 'to * level' will find all mods that contain 'to ' \
               followed by anything followed by ' level'. Search is \
               case-insensitive."
            ()
        in
        `find pattern
      );
      (
        Clap.case "write-default-costs"
          ~description:
            "Output default costs to data/costs.json. Warning: if you \
             customized this file, all your changes will be lost."
        @@ fun () ->
        `write_default_costs
      );
      (
        Clap.case "write-ninja-costs"
          ~description:
            "Read costs from poe.ninja's API and output them to data/costs.json.
             Warning: if you customized this file, all your changes will be lost."
        @@ fun () ->
        let league =
          Clap.default_string
            ~placeholder: "LEAGUE"
            ~description: "League name to give to poe.ninja's API."
            "Archnemesis"
        in
        `write_poe_ninja_costs league
      );
    ]
  in
  Clap.close ();
  match command with
    | `format filename ->
        let (_, recipe) = parse_recipe filename in
        Pretext.to_channel ~starting_level: 2 stdout (AST.pp recipe)
    | `run (filename, count, verbose, seed, show_seed) ->
        let (preamble, recipe) = parse_recipe filename in
        let compiled_recipe = Linear.compile recipe in
        let compile_function library (fname, argument_list, body) =
          Interpreter.Library.add fname (argument_list, Linear.compile body) library
        in
        let preamble =
          List.fold_left compile_function Interpreter.Library.empty preamble
        in
        load ();
        Option.iter Random.init seed;
        if show_seed then (
          let seed =
            match seed with
              | None ->
                  (* According to the doc, the argument of Random.int
                     must be less than 2^30. *)
                  let seed = Random.int 0x3fffffff in
                  Random.init seed;
                  seed
              | Some seed ->
                  seed
          in
          echo "Seed: %d" seed
        );
        run_recipe preamble compiled_recipe ~count ~verbose
    | `compile filename ->
        let (_, recipe) = parse_recipe filename in
        let compiled_recipe = Linear.compile recipe in
        let decompiled_recipe = Linear.decompile compiled_recipe in
        let output decompiled_recipe =
          Pretext.to_channel ~starting_level: 2 stdout (AST.pp decompiled_recipe)
        in
        Option.iter output decompiled_recipe
    | `find pattern ->
        load ();
        find pattern
    | `write_default_costs ->
        if not (Sys.file_exists "data") then Sys.mkdir "data" 0755;
        Cost.write_defaults costs_json_filename
    | `write_poe_ninja_costs league ->
        if not (Sys.file_exists "data") then Sys.mkdir "data" 0755;
        Ninja.write_costs ~league ~filename: costs_json_filename

let backtrace = false

let () =
  Random.self_init ();
  if backtrace then Printexc.record_backtrace true;
  try
    main ()
  with exn ->
    echo "Entering directory '%s'" (Sys.getcwd ());
    if backtrace then Printf.eprintf "%s\n" (Printexc.get_backtrace ());
    Printf.eprintf "%s" (Printexc.to_string exn);
    exit 1

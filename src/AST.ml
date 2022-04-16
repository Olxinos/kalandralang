open Misc

module Label:
sig
  type t
  val compare: t -> t -> int
  val make: string -> t
  val fresh: string -> t
  val show: t -> string
end = struct
  type t = { int: int; string: string }

  let compare a b = Int.compare a.int b.int

  let string_table: (string, t) Hashtbl.t = Hashtbl.create 128
  let next = ref 0
  let next_for_fresh: (string, int) Hashtbl.t = Hashtbl.create 128

  let make string =
    match Hashtbl.find_opt string_table string with
      | None ->
          let int = !next in
          incr next;
          let id = { int; string } in
          Hashtbl.replace string_table string id;
          id
      | Some id ->
          id

  let fresh string =
    let index = Hashtbl.find_opt next_for_fresh string |> default 1 in
    Hashtbl.replace next_for_fresh string (index + 1);
    make (".__" ^ string ^ string_of_int index)

  let show lbl = lbl.string
end

type 'a node =
  {
    loc: Lexing.position * Lexing.position;
    node: 'a;
  }

type eldritch_currency_tier =
  | Lesser
  | Greater
  | Grand
  | Exceptional

let eldritch_tier_of_currency: eldritch_currency_tier -> Mod.eldritch_tier = function
  | Lesser -> Lesser
  | Greater -> Greater
  | Grand -> Grand
  | Exceptional -> Exceptional

(* TODO: orb of conflict (or is it dominance? the maven one) *)
(* TODO: show_veiled_mod_pool *)
(* TODO: harvest reforge lucky *)
(* TODO: harvest reforge keep prefix/suffix lucky *)
(* TODO: conditions on rarity *)
(* TODO: conditions on number of influenced modifiers *)
type currency =
  | Orb_of_transmutation
  | Orb_of_augmentation
  | Orb_of_alteration
  | Regal_orb
  | Orb_of_alchemy
  | Orb_of_scouring
  | Blessed_orb
  | Chaos_orb
  | Orb_of_annulment
  | Exalted_orb
  | Crusader_exalted_orb
  | Hunter_exalted_orb
  | Redeemer_exalted_orb
  | Warlord_exalted_orb
  | Veiled_chaos_orb
  | Essence of Essence.name
  | Fossils of Fossil.t list
  | Awakeners_orb
  | Ember of eldritch_currency_tier
  | Ichor of eldritch_currency_tier
  | Eldritch_annul
  | Eldritch_exalt
  | Eldritch_chaos
  (* Not really currencies but... *)
  | Harvest_augment of Mod.harvest_tag
  | Harvest_non_to of Mod.harvest_tag
  | Harvest_reforge of Mod.harvest_tag
  | Harvest_reforge_keep_prefixes
  | Harvest_reforge_keep_suffixes
  | Beastcraft_aspect_of_the_avian
  | Beastcraft_aspect_of_the_cat
  | Beastcraft_aspect_of_the_crab
  | Beastcraft_aspect_of_the_spider
  | Beastcraft_split
  | Beastcraft_imprint
  | Aisling
  | Craft of Id.t
  | Multimod
  | Prefixes_cannot_be_changed
  | Suffixes_cannot_be_changed
  | Cannot_roll_attack_mods
  | Cannot_roll_caster_mods
  | Remove_crafted_mods
  | Craft_any_prefix
  | Craft_any_suffix

let show_currency = function
  | Orb_of_transmutation -> "transmute"
  | Orb_of_augmentation -> "augment"
  | Orb_of_alteration -> "alt"
  | Regal_orb -> "regal"
  | Orb_of_alchemy -> "alch"
  | Orb_of_scouring -> "scour"
  | Blessed_orb -> "bless"
  | Chaos_orb -> "chaos"
  | Orb_of_annulment -> "annul"
  | Exalted_orb -> "exalt"
  | Crusader_exalted_orb -> "crusader_exalt"
  | Hunter_exalted_orb -> "hunter_exalt"
  | Redeemer_exalted_orb -> "redeemer_exalt"
  | Warlord_exalted_orb -> "warlord_exalt"
  | Veiled_chaos_orb -> "veiled_chaos"
  | Essence name -> "essence_of_" ^ Essence.show_name name
  | Fossils fossils -> String.concat " + " (List.map Fossil.show fossils)
  | Awakeners_orb -> "awaken"
  | Ember Lesser -> "lesser_ember"
  | Ember Greater -> "greater_ember"
  | Ember Grand -> "grand_ember"
  | Ember Exceptional -> "exceptional_ember"
  | Ichor Lesser -> "lesser_ichor"
  | Ichor Greater -> "greater_ichor"
  | Ichor Grand -> "grand_ichor"
  | Ichor Exceptional -> "exceptional_ichor"
  | Eldritch_annul -> "eldritch_annul"
  | Eldritch_exalt -> "eldritch_exalt"
  | Eldritch_chaos -> "eldritch_chaos"
  | Harvest_augment tag -> "harvest_augment_" ^ Id.show (Mod.tag_id tag)
  | Harvest_non_to tag -> let t = Id.show (Mod.tag_id tag) in "harvest_non_" ^ t ^ "_to_" ^ t
  | Harvest_reforge tag -> "harvest_reforge_" ^ Id.show (Mod.tag_id tag)
  | Harvest_reforge_keep_prefixes -> "harvest_reforge_keep_prefixes"
  | Harvest_reforge_keep_suffixes -> "harvest_reforge_keep_suffixes"
  | Beastcraft_aspect_of_the_avian -> "beastcraft_aspect_of_the_avian"
  | Beastcraft_aspect_of_the_cat -> "beastcraft_aspect_of_the_cat"
  | Beastcraft_aspect_of_the_crab -> "beastcraft_aspect_of_the_crab"
  | Beastcraft_aspect_of_the_spider -> "beastcraft_aspect_of_the_spider"
  | Beastcraft_split -> "beastcraft_split"
  | Beastcraft_imprint -> "beastcraft_imprint"
  | Aisling -> "aisling"
  | Craft id -> Printf.sprintf "craft %S" (Id.show id)
  | Multimod -> "multimod"
  | Prefixes_cannot_be_changed -> "prefixes_cannot_be_changed"
  | Suffixes_cannot_be_changed -> "suffixes_cannot_be_changed"
  | Cannot_roll_attack_mods -> "cannot_roll_attack_mods"
  | Cannot_roll_caster_mods -> "cannot_roll_caster_mods"
  | Remove_crafted_mods -> "remove_crafted_mods"
  | Craft_any_prefix -> "craft_any_prefix"
  | Craft_any_suffix -> "craft_any_suffix"

let pp_currency currency = Pretext.atom (show_currency currency)

type amount = (int * currency) list

let pp_one_amount (n, c) =
  Pretext.box [ Pretext.int n; Pretext.space; pp_currency c ]

let pp_amount amount =
  Pretext.box [ Pretext.(separate_map space) pp_one_amount amount ]

type buy_with =
  {
    modifier: Id.t;
    fractured: bool;
  }

type buy =
  {
    influence: Influence.t;
    base: Id.t;
    ilvl: int;
    mods: buy_with list;
    cost: amount;
    (* TODO: {min,max}_{pre,suf}fix *)
  }

let pp_string string =
  Pretext.atom ("\"" ^ string ^ "\"")

let pp_sec_influence (influence: Influence.sec) =
  let open Pretext in
  match influence with
    | Shaper -> atom "shaper"
    | Elder -> atom "elder"
    | Crusader -> atom "crusader"
    | Hunter -> atom "hunter"
    | Redeemer -> atom "redeemer"
    | Warlord -> atom "warlord"

let pp_buy { influence; base; ilvl; mods; cost } =
  let open Pretext in
  let pp_with buy_with =
    if buy_with.fractured then
      box [ atom "with"; space; atom "fractured"; space; Id.pp buy_with.modifier ]
    else
      box [ atom "with"; space; Id.pp buy_with.modifier ]
  in
  box [
    box [
      atom "buy"; space;
      (
        match influence with
          | Not_influenced ->
              empty
          | Fractured ->
              seq [ atom "fractured"; space ]
          | Synthesized ->
              seq [ atom "synthesized"; space ]
          | SEC sec ->
              seq [ pp_sec_influence sec; space ]
          | SEC_pair (sec1, sec2) ->
              seq [ pp_sec_influence sec1; space; pp_sec_influence sec2; space ]
          | Exarch ->
              seq [ atom "exarch"; space ]
          | Eater ->
              seq [ atom "eater"; space ]
          | Exarch_and_eater ->
              seq [ atom "exarch"; space; atom "eater"; space ]
      );
      Id.pp base;
    ];
    break;
    indent;
    box [ atom "ilvl"; space; int ilvl ]; break;
    separate_map break pp_with mods; break;
    (
      match cost with
        | [] -> empty
        | _ :: _ -> box [ atom "for"; space; pp_amount cost ]
    );
    dedent;
  ]

type buy_being_made =
  {
    bbm_influence: Influence.t;
    bbm_base: Id.t option;
    bbm_ilvl: int option;
    bbm_mods: buy_with list;
    bbm_cost: amount option;
    (* TODO: {min,max}_{pre,suf}fix *)
  }

type buy_argument =
  | BA_influence of Influence.t
  | BA_base of Id.t
  | BA_ilvl of int
  | BA_with of buy_with
  | BA_for of amount

let make_buy args =
  let handle_arg bbm = function
    | BA_influence influence ->
        let bbm_influence = Influence.add bbm.bbm_influence influence in
        { bbm with bbm_influence }
    | BA_base id ->
        let bbm_base =
          match bbm.bbm_base with
            | None -> Some id
            | Some _ -> fail "buy: cannot specify more than one base type id"
        in
        { bbm with bbm_base }
    | BA_ilvl ilvl ->
        let bbm_ilvl =
          match bbm.bbm_ilvl with
            | None -> Some ilvl
            | Some _ -> fail "buy: cannot specify more than one ilvl"
        in
        { bbm with bbm_ilvl }
    | BA_with buy_with ->
        { bbm with bbm_mods = buy_with :: bbm.bbm_mods }
    | BA_for cost ->
        let bbm_cost =
          match bbm.bbm_cost with
            | None -> Some cost
            | Some _ -> fail "buy: cannot specify more than one cost"
        in
        { bbm with bbm_cost }
  in
  let empty_bbm =
    {
      bbm_influence = Not_influenced;
      bbm_base = None;
      bbm_ilvl = None;
      bbm_mods = [];
      bbm_cost = None;
    }
  in
  let bbm = List.fold_left handle_arg empty_bbm args in
  {
    influence = bbm.bbm_influence;
    base = (
      match bbm.bbm_base with
        | None -> fail "buy: missing base type id"
        | Some base -> base
    );
    ilvl = bbm.bbm_ilvl |> default 100;
    mods = List.rev bbm.bbm_mods;
    cost = bbm.bbm_cost |> default [];
  }

type arithmetic_expression =
  | Constant of int
  | Variable of string
  | Get_min of Id.t
  | Get_max of Id.t
  | Base of Id.t
  | Function_call of string * arithmetic_expression list
  | Sum of arithmetic_expression * arithmetic_expression
  | Product of arithmetic_expression * arithmetic_expression
  | Difference of arithmetic_expression * arithmetic_expression
  | Quotient of arithmetic_expression * arithmetic_expression

type condition =
  (* Operators *)
  | True
  | False
  | Not of condition
  | And of condition * condition
  | Or of condition * condition
  (* Item Conditions *)
  | Has of Id.t
  | Prefix_count of int * int
  | Open_prefix
  | Full_prefixes
  | Suffix_count of int * int
  | Open_suffix
  | Full_suffixes
  | Is_equal of arithmetic_expression * arithmetic_expression
  | Greater_than of arithmetic_expression * arithmetic_expression
  | Greater_equal of arithmetic_expression * arithmetic_expression
  | Less_than of arithmetic_expression * arithmetic_expression
  | Less_equal of arithmetic_expression * arithmetic_expression

let maybe_parentheses use_parentheses document =
  let open Pretext in
  if use_parentheses then
    box [ atom "("; break0; indent; document; dedent; break0; atom ")" ]
  else
    document

let rec pp_arithmetic_expression ?(ctx = `top) expression =
  let open Pretext in
  match expression with
    | Constant x -> int x
    | Variable x -> atom x
    | Get_min state_id -> seq [atom "getmin"; space; atom (Id.show state_id)]
    | Get_max state_id -> seq [atom "getmax"; space; atom (Id.show state_id)]
    | Base state_id -> seq [atom "base"; space; atom (Id.show state_id)]
    | Sum (lhs, rhs) ->
        let parentheses =
          match ctx with
            | `top -> false
            | `product_or_quotient | `right_hand_side -> true
          in
            maybe_parentheses parentheses @@ seq [
              pp_arithmetic_expression lhs; space; atom "+"; break;
              pp_arithmetic_expression ~ctx: `right_hand_side rhs
            ]
    | Product (lhs, rhs) ->
        let parentheses =
          match ctx with
            | `top | `product_or_quotient -> false
            | `right_hand_side -> true
          in
            maybe_parentheses parentheses @@ seq [
              pp_arithmetic_expression ~ctx: `product_or_quotient lhs; space; atom "*"; break;
              pp_arithmetic_expression ~ctx: `right_hand_side rhs
            ]
    | Difference (lhs, rhs) ->
        let parentheses =
          match ctx with
            | `top -> false
            | `product_or_quotient | `right_hand_side -> true
          in
            maybe_parentheses parentheses @@ seq [
              pp_arithmetic_expression lhs; space; atom "-"; break;
              pp_arithmetic_expression ~ctx: `right_hand_side rhs
            ]
    | Quotient (lhs, rhs) ->
        let parentheses =
          match ctx with
            | `top | `product_or_quotient -> false
            | `right_hand_side -> true
          in
            maybe_parentheses parentheses @@ seq [
              pp_arithmetic_expression ~ctx: `product_or_quotient lhs; space; atom "/"; break;
              pp_arithmetic_expression ~ctx: `right_hand_side rhs
            ]
    | Function_call (name, argument_list) ->
        seq [atom name; space; pp_argument_list argument_list]
and pp_argument_list = function
  | [] -> Pretext.empty
  | argument::argument_list ->
      Pretext.seq [pp_arithmetic_expression argument; Pretext.break; pp_argument_list argument_list]

let rec pp_condition ?(ctx = `top) condition =
  let open Pretext in
  match condition with
    | True ->
        atom "true"
    | False ->
        atom "false"
    | Not a ->
        seq [ atom "not"; space; pp_condition ~ctx: `not a ]
    | And (a, b) ->
        let parentheses =
          match ctx with
            | `top | `or_ | `and_ -> false
            | `not -> true
        in
        maybe_parentheses parentheses @@ seq [
          pp_condition ~ctx: `and_ a; space; atom "and"; break;
          pp_condition ~ctx: `and_ b
        ]
    | Or (a, b) ->
        let parentheses =
          match ctx with
            | `top | `or_ -> false
            | `not | `and_ -> true
        in
        maybe_parentheses parentheses @@ seq [
          pp_condition ~ctx: `and_ a; space; atom "or"; break;
          pp_condition ~ctx: `and_ b
        ]
    | Has modifier ->
        seq [ atom "has"; space; Id.pp modifier ]
    | Prefix_count (0, 0) ->
        atom "no_prefix"
    | Prefix_count (a, b) when a = b ->
        seq [ atom "prefix_count"; space; int a ]
    | Prefix_count (a, b) ->
        seq [ atom "prefix_count"; space; int a; atom "-"; int b ]
    | Open_prefix ->
        atom "open_prefix"
    | Full_prefixes ->
        atom "full_prefixes"
    | Suffix_count (0, 0) ->
        atom "no_suffix"
    | Suffix_count (a, b) when a = b ->
        seq [ atom "suffix_count"; space; int a ]
    | Suffix_count (a, b) ->
        seq [ atom "suffix_count"; space; int a; atom "-"; int b ]
    | Open_suffix ->
        atom "open_suffix"
    | Full_suffixes ->
        atom "full_suffixes"
    | Is_equal (lhs, rhs) ->
        seq [ pp_arithmetic_expression lhs; space; atom "=="; space; pp_arithmetic_expression rhs]
    | Greater_than (lhs, rhs) ->
        seq [ pp_arithmetic_expression lhs; space; atom ">"; space; pp_arithmetic_expression rhs]
    | Greater_equal (lhs, rhs) ->
        seq [ pp_arithmetic_expression lhs; space; atom ">="; space; pp_arithmetic_expression rhs]
    | Less_than (lhs, rhs) ->
        seq [ pp_arithmetic_expression lhs; space; atom "<"; space; pp_arithmetic_expression rhs]
    | Less_equal (lhs, rhs) ->
        seq [ pp_arithmetic_expression lhs; space; atom "<="; space; pp_arithmetic_expression rhs]

type simple_instruction =
  | Goto of Label.t
  | Stop
  | Buy of buy
  | Apply of currency
  | Set_aside
  | Swap
  | Use_imprint
  | Gain of amount
  | Echo of string
  | Echo_int of arithmetic_expression
  | Show
  | Show_mod_pool
  | Return of arithmetic_expression
  | Assignment of string * arithmetic_expression

let pp_simple_instruction instruction =
  let open Pretext in
  match instruction with
    | Goto label ->
        seq [ atom "goto"; space; atom (Label.show label) ]
    | Stop ->
        atom "stop"
    | Buy buy ->
        pp_buy buy
    | Apply currency ->
        pp_currency currency
    | Set_aside ->
        atom "set_aside"
    | Swap ->
        atom "swap"
    | Use_imprint ->
        atom "use_imprint"
    | Gain amount ->
        seq [ atom "gain"; space; pp_amount amount ]
    | Echo message ->
        box [ atom "echo"; space; pp_string message ]
    | Echo_int expression ->
        box [ atom "echo"; space; pp_arithmetic_expression expression ]
    | Return expression ->
        seq [ atom "gain"; space; pp_arithmetic_expression expression ]
    | Assignment (variable, expression) ->
        seq [ atom variable; space; atom "="; space; pp_arithmetic_expression expression ]
    | Show ->
        atom "show"
    | Show_mod_pool ->
        atom "show_mod_pool"

type instruction_node =
  | Noop
  | Seq of instruction_node node * instruction_node node
  | Label of Label.t
  | Simple of simple_instruction
  | If of condition * instruction_node node * instruction_node node option
  | Until of condition * instruction_node node
  | While of condition * instruction_node node
  | Repeat of instruction_node node * condition

type function_declaration = string * string list * instruction_node node
type preamble = function_declaration list
type t = preamble * instruction_node node

let block contents =
  let open Pretext in
  seq [
    Pretext.(if_flat empty (concat space (atom "{")));
    indent; break; contents; break; dedent;
    Pretext.(if_flat empty (concat (atom "}") newline));
  ]

let rec pp_instruction_node instruction =
  let open Pretext in
  match instruction with
    | Noop ->
        empty
    | Seq (x, { node = Noop; _ })
    | Seq ({ node = Noop; _ }, x) ->
        pp_instruction_node x.node
    | Seq (a, b) ->
        seq [
          pp_instruction a;
          newline;
          pp_instruction b;
        ]
    | Label lbl ->
        seq [ empty_line; dedent; atom (Label.show lbl); atom ":"; indent ]
    | Simple instruction ->
        pp_simple_instruction instruction
    | If (condition, then_, None) ->
        box [
          box [
            atom "if";
            indent; break; pp_condition condition; break; dedent;
            atom "then";
          ];
          block (pp_instruction then_);
        ]
    | If (condition, then_, Some else_) ->
        box [
          box [
            box [
              atom "if";
              indent; break; pp_condition condition; break; dedent;
              atom "then";
            ];
            block (pp_instruction then_);
            atom "else";
          ];
          indent; break; pp_instruction else_; break; dedent;
        ]
    | Until (condition, body) ->
        box [
          box [
            atom "until";
            indent; break; pp_condition condition; break; dedent;
            atom "do";
          ];
          block (pp_instruction body);
        ]
    | While (condition, body) ->
        box [
          box [
            atom "while";
            indent; break; pp_condition condition; break; dedent;
            atom "do";
          ];
          block (pp_instruction body);
        ]
    | Repeat (body, condition) ->
        box [
          box [
            atom "repeat";
            indent; break; pp_instruction body; break; dedent;
            atom "until";
          ];
          block (pp_condition condition);
        ]

and pp_instruction instruction =
  pp_instruction_node instruction.node

let pp = pp_instruction

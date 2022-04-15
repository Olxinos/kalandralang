open Misc

(* There are other generation types ("corrupted", "exarch_implicit", ...)
   but we only support the following ones. *)
type generation_type =
  | Prefix
  | Suffix

let show_generation_type = function
  | Prefix -> "Prefix"
  | Suffix -> "Suffix"

let pp_generation_type generation_type =
  Pretext.OCaml.variant (show_generation_type generation_type) []

type stat =
  {
    id: Id.t;
    min: int;
    max: int;
    current: int;
  }

type t =
  {
    id: Id.t;
    domain: Base_item.domain;
    generation_type: generation_type;
    (* Only one mod per [group] can spawn on an item. *)
    group: Id.t;
    required_level: int;
    (* [spawn_weights] define on which items the modifier can spawn,
       and with which weight (first matching tag in the list wins).
       Those weights can further be modified by [generation_weights].
       Influenced items have different tags than regular items. *)
    spawn_weights: (Id.t * int) list;
    (* [generation_weights] are percentage multipliers to apply
       if some tags are present. *)
    generation_weights: (Id.t * int) list;
    (* [tags] define the tags for fossil and harvest crafting. *)
    tags: Id.Set.t;
    (* [adds_tags] causes the item to now have those tags.
       This can thus change future spawn weights. *)
    adds_tags: Id.Set.t;
    stats: stat list;
  }

let choose_randomly (min, max) =
  if max >= min then
    min + Random.int (max - min + 1)
  else
    max + Random.int (min - max + 1)

let roll_stat stat : stat =
  { stat with current = choose_randomly (stat.min, stat.max) }

let roll_modifier modifier : t =
  { modifier with stats = List.map roll_stat modifier.stats }

let get_stat_value ?(acc = 0) (modifier : t) (stat_id : Id.t) : int =
  let accumulate acc (stat : stat) =
    if compare stat.id stat_id == 0 then acc + stat.current else acc
  in
    List.fold_left accumulate acc modifier.stats

let is_prefix modifier =
  match modifier.generation_type with
    | Prefix ->
        true
    | _ ->
        false

let is_suffix modifier =
  match modifier.generation_type with
    | Suffix ->
        true
    | _ ->
        false

let is_prefix_or_suffix modifier =
  (* Currently always true but will change when we add eldritch implicits. *)
  match modifier.generation_type with
    | Prefix | Suffix ->
        true

let is_crafted modifier =
  match modifier.domain with
    | Crafted ->
        true
    | _ ->
        false

let caster_tag = Id.make "caster"
let attack_tag = Id.make "attack"

let is_caster modifier =
  Id.Set.mem caster_tag modifier.tags

let is_attack modifier =
  Id.Set.mem attack_tag modifier.tags

let pp_spawn_weight (tag, weight) =
  Pretext.OCaml.tuple [ Id.pp tag; Pretext.OCaml.int weight ]

let pp_stat { id; min; max; _ } =
  Pretext.OCaml.record [
    "id", Id.pp id;
    "min", Pretext.OCaml.int min;
    "max", Pretext.OCaml.int max;
  ]

let pp {
    id; domain; generation_type; group; required_level;
    spawn_weights; generation_weights;
    tags; adds_tags; stats;
  } =
  Pretext.OCaml.record [
    "id", Id.pp id;
    "domain", Base_item.pp_domain domain;
    "generation_type", pp_generation_type generation_type;
    "group", Id.pp group;
    "required_level", Pretext.OCaml.int required_level;
    "spawn_weights", Pretext.OCaml.list pp_spawn_weight spawn_weights;
    "generation_weights", Pretext.OCaml.list pp_spawn_weight generation_weights;
    "tags", Id.Set.pp tags;
    "adds_tags", Id.Set.pp adds_tags;
    "stats", Pretext.OCaml.list pp_stat stats;
  ]

let pool = ref []
let id_map = ref Id.Map.empty

type data = t list
let export (): data = !pool
let import (x: data) =
  pool := x;
  List.iter (fun modifier -> id_map := Id.Map.add modifier.id modifier !id_map) x

let royale_rex = rex "Royale"

let load filename =
  let as_spawn_weight json =
    let tag = ref None in
    let weight = ref 0 in
    let handle_value (field, value) =
      match field with
        | "tag" ->
            tag := Some (JSON.as_id value)
        | "weight" ->
            weight := JSON.as_int value
        | _ ->
            ()
    in
    List.iter handle_value (JSON.as_object json);
    let tag =
      match !tag with
        | None ->
            fail "%s: no tag" (JSON.show_origin json)
        | Some tag ->
            tag
    in
    tag, !weight
  in
  let add_entry (id, json) =
    if id =~! royale_rex then
      let id = Id.make id in
      let domain = ref None in
      let generation_type = ref None in
      let group = ref Id.empty in
      let required_level = ref 0 in
      let spawn_weights = ref [] in
      let generation_weights = ref [] in
      let tags = ref Id.Set.empty in
      let adds_tags = ref Id.Set.empty in
      let stats = ref [] in
      let handle_value (field, value) =
        match field with
          | "domain" ->
              domain := Base_item.as_domain value
          | "generation_type" ->
              (
                match JSON.as_string value with
                  | "prefix" ->
                      generation_type := Some Prefix
                  | "suffix" ->
                      generation_type := Some Suffix
                  | _ ->
                      ()
              )
          | "group" ->
              group := JSON.as_id value
          | "required_level" ->
              required_level := JSON.as_int value
          | "spawn_weights" ->
              spawn_weights := JSON.as_array value |> List.map as_spawn_weight
          | "generation_weights" ->
              generation_weights := JSON.as_array value |> List.map as_spawn_weight
          | "implicit_tags" ->
              tags := JSON.as_array value |> List.map JSON.as_id |> Id.Set.of_list
          | "adds_tags" ->
              adds_tags := JSON.as_array value |> List.map JSON.as_id |> Id.Set.of_list
          | "stats" ->
              let as_stat json =
                let id = ref Id.empty in
                let min = ref 0 in
                let max = ref 0 in
                let current = 0 in
                let handle_value (field, value) =
                  match field with
                    | "id" -> id := JSON.as_id value
                    | "min" -> min := JSON.as_int value
                    | "max" -> max := JSON.as_int value
                    | _ -> ()
                in
                List.iter handle_value (JSON.as_object json);
                {
                  id = !id;
                  min = !min;
                  max = !max;
                  current = current;
                }
              in
              stats := JSON.as_array value |> List.map as_stat
          | _ ->
              ()
      in
      List.iter handle_value (JSON.as_object json);
      match !generation_type, !domain with
        | None, _ | _, None ->
            ()
        | Some (Prefix | Suffix as generation_type), Some domain ->
            let modifier =
              {
                id;
                domain;
                generation_type;
                group = !group;
                required_level = !required_level;
                spawn_weights = !spawn_weights;
                generation_weights = !generation_weights;
                tags = !tags;
                adds_tags = !adds_tags;
                stats = !stats;
              }
            in
            match Id.Map.find_opt id !id_map with
              | Some _ ->
                  fail "%s: two items with id %s" filename (Id.show id)
              | None ->
                  pool := modifier :: !pool;
                  id_map := Id.Map.add id modifier !id_map
  in
  List.iter add_entry JSON.(parse_file filename |> as_object)

let by_id id =
  match Id.Map.find_opt id !id_map with
    | None ->
        fail "no mod with id %S" (Id.show id)
    | Some x ->
        x

type show_mode =
  | With_placeholders
  | With_ranges
  | With_random_values
  | With_current_values

let show ?(indentation = 0) ?(fractured = false) mode modifier =
  let generation_type =
    match modifier.generation_type with
      | Prefix -> "(prefix) "
      | Suffix -> "(suffix) "
  in
  let fractured = if fractured then "{fractured} " else "" in
  let domain =
    match modifier.domain with
      | Item | Misc | Abyss_jewel | Affliction_jewel -> ""
      | Crafted -> "{crafted} "
      | Veiled -> "{veiled} "
  in
  let margin =
    String.make (indentation + String.length generation_type + String.length domain) ' '
  in
  let translated_mod =
    match modifier.domain with
      | Veiled ->
          "????????"
      | _ ->
          let translate_mode: Stat_translation.translate_mode =
            let ranges =
              let add_range acc (stat: stat) = Id.Map.add stat.id (stat.min, stat.max) acc in
              List.fold_left add_range Id.Map.empty modifier.stats
            in
            match mode with
              | With_placeholders ->
                  With_placeholders ranges
              | With_ranges ->
                  With_ranges ranges
              | With_random_values ->
                  With_values (Id.Map.map choose_randomly ranges)
              | With_current_values ->
                  let add_value acc (stat : stat) = Id.Map.add stat.id stat.current acc in
                  With_values (List.fold_left add_value Id.Map.empty modifier.stats)
          in
          let strings =
            let ids = List.map (fun (stat: stat) -> stat.id) modifier.stats in
            List.map (Stat_translation.translate_id translate_mode) ids
          in
          let strings = deduplicate String.compare strings in
          String.concat ("\n" ^ margin) strings
  in
  generation_type ^ fractured ^ domain ^ translated_mod ^ " (" ^ Id.show modifier.id ^ ")"

let multimod_id = Id.make "StrIntMasterItemGenerationCanHaveMultipleCraftedMods"
let prefixes_cannot_be_changed_id = Id.make "StrMasterItemGenerationCannotChangePrefixes"
let suffixes_cannot_be_changed_id = Id.make "DexMasterItemGenerationCannotChangeSuffixes"
let cannot_roll_attack_mods_id = Id.make "IntMasterItemGenerationCannotRollAttackAffixes"
let cannot_roll_caster_mods_id = Id.make "StrDexMasterItemGenerationCannotRollCasterAffixes"

let beastcrafted_avian_aspect_id = Id.make "GrantsBirdAspectCrafted" (* Saqawal *)
let beastcrafted_cat_aspect_id = Id.make "GrantsCatAspectCrafted" (* Farrul *)
let beastcrafted_crab_aspect_id = Id.make "GrantsCrabAspectCrafted" (* Craiceann *)
let beastcrafted_spider_aspect_id = Id.make "GrantsSpiderAspectCrafted" (* Fenumus *)

let veiled_prefix_id = Id.make "VeiledPrefix"
let veiled_suffix_id = Id.make "VeiledSuffix"
let veiled_prefix_group = Id.make "VeiledPrefix"
let veiled_suffix_group = Id.make "VeiledSuffix"

type harvest_tag = [
  | `attack
  | `caster
  | `chaos
  | `cold
  | `critical
  | `defences
  | `fire
  | `life
  | `lightning
  | `physical
  | `speed
]

type tag = [
  | harvest_tag
  | `minion
  | `aura
  | `curse
  | `bleed
  | `poison
  | `elemental
  | `gem
  | `mana
  | `attribute
]

let attack_tag_id = Id.make "attack"
let caster_tag_id = Id.make "caster"
let chaos_tag_id = Id.make "chaos"
let cold_tag_id = Id.make "cold"
let critical_tag_id = Id.make "critical"
let defences_tag_id = Id.make "defences"
let fire_tag_id = Id.make "fire"
let life_tag_id = Id.make "life"
let lightning_tag_id = Id.make "lightning"
let physical_tag_id = Id.make "physical"
let speed_tag_id = Id.make "speed"
let minion_id = Id.make "minion"
let aura_id = Id.make "aura"
let curse_id = Id.make "curse"
let bleed_id = Id.make "bleed"
let poison_id = Id.make "poison"
let elemental_id = Id.make "elemental"
let gem_id = Id.make "gem"
let mana_id = Id.make "mana"
let attribute_id = Id.make "attribute"

let tag_id: [< tag ] -> Id.t = function
  | `attack -> attack_tag_id
  | `caster -> caster_tag_id
  | `chaos -> chaos_tag_id
  | `cold -> cold_tag_id
  | `critical -> critical_tag_id
  | `defences -> defences_tag_id
  | `fire -> fire_tag_id
  | `life -> life_tag_id
  | `lightning -> lightning_tag_id
  | `physical -> physical_tag_id
  | `speed -> speed_tag_id
  | `minion -> minion_id
  | `aura -> aura_id
  | `curse -> curse_id
  | `bleed -> bleed_id
  | `poison -> poison_id
  | `elemental -> elemental_id
  | `gem -> gem_id
  | `mana -> mana_id
  | `attribute -> attribute_id

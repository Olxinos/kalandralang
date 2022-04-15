open Misc

type rarity =
  | Normal
  | Magic
  | Rare

let show_rarity = function
  | Normal -> "Normal"
  | Magic -> "Magic"
  | Rare -> "Rare"

let pp_rarity rarity =
  Pretext.OCaml.string (show_rarity rarity)

type modifier =
  {
    modifier: Mod.t;
    fractured: bool;
  }

let pp_modifier { modifier; fractured } =
  Pretext.OCaml.record [
    "modifier", Mod.pp modifier;
    "fractured", Pretext.OCaml.bool fractured;
  ]

type t =
  {
    base: Base_item.t;
    level: int;
    (* [tags] can expand or restrict the mod pool further (e.g. influences). *)
    tags: Id.Set.t;
    rarity: rarity;
    mods: modifier list;
    split: bool;
  }

let get_stat_value (stat_id : Id.t) (item : t) : int =
  let accumulate acc modifier =
    Mod.get_stat_value ~acc: acc modifier.modifier stat_id
  in
    List.fold_left accumulate 0 item.mods

let local_physical_damage_percent_increase item : int =
  (* assumes 20% quality *)
  20 + get_stat_value (Id.make "local_physical_damage_+%") item

let local_flat_min_physical_increase item : int =
  get_stat_value (Id.make "local_minimum_added_physical_damage") item

let local_flat_max_physical_increase item : int =
  get_stat_value (Id.make "local_maximum_added_physical_damage") item

let local_attack_speed_increase item : int =
  get_stat_value (Id.make "local_attack_speed_+%") item

let local_flat_min_cold_damage_increase item : int =
  get_stat_value (Id.make "local_minimum_added_cold_damage") item

let local_flat_min_fire_damage_increase item : int =
  get_stat_value (Id.make "local_minimum_added_fire_damage") item

let local_flat_min_lightning_damage_increase item : int =
  get_stat_value (Id.make "local_minimum_added_lightning_damage") item

let local_flat_max_cold_damage_increase item : int =
  get_stat_value (Id.make "local_maximum_added_cold_damage") item

let local_flat_max_fire_damage_increase item : int =
  get_stat_value (Id.make "local_maximum_added_fire_damage") item

let local_flat_max_lightning_damage_increase item : int =
  get_stat_value (Id.make "local_maximum_added_lightning_damage") item

let calculate_pdps item : float =
  let total_flat_phys =
    float_of_int @@
        Base_item.min_physical_damage item.base
      + Base_item.max_physical_damage item.base
      + local_flat_min_physical_increase item
      + local_flat_max_physical_increase item
  and total_phys_multiplier =
    (100. +. (float_of_int @@ local_physical_damage_percent_increase item)) /. 100.
  and base_attack_time =
    float_of_int @@ Base_item.attack_time_in_milliseconds item.base
  and speed_multiplier =
    (100. +. (float_of_int @@ local_attack_speed_increase item)) /. 100.
  in
    total_flat_phys *. total_phys_multiplier *. speed_multiplier *. 500. /. base_attack_time

let calculate_edps item : float =
  let total_flat_elemental =
    float_of_int @@
        local_flat_min_cold_damage_increase item
      + local_flat_min_fire_damage_increase item
      + local_flat_min_lightning_damage_increase item
      + local_flat_max_cold_damage_increase item
      + local_flat_max_fire_damage_increase item
      + local_flat_max_lightning_damage_increase item
    and base_attack_time =
      float_of_int @@ Base_item.attack_time_in_milliseconds item.base
    and speed_multiplier =
      (100. +. (float_of_int @@ local_attack_speed_increase item)) /. 100.
    in
      total_flat_elemental *. speed_multiplier *. 500. /. base_attack_time

let calculate_dps item : float =
  (calculate_pdps item) +. (calculate_edps item)

let pp { base; level; tags; rarity; mods; split } =
  Pretext.OCaml.record [
    "base", Base_item.pp base;
    "level", Pretext.OCaml.int level;
    "tags", Id.Set.pp tags;
    "rarity", pp_rarity rarity;
    "mods", Pretext.OCaml.list pp_modifier mods;
    "split", Pretext.OCaml.bool split;
  ]

let show_modifier { modifier; fractured } =
  Mod.show ~fractured With_current_values modifier

let has_influence influence item =
  match Base_tag.get_influence_tag_for_tags item.base.tags influence with
    | None ->
        false
    | Some influence_tag ->
        Id.Set.mem influence_tag item.tags

let has_any_influence item =
  List.exists (fun influence -> has_influence influence item) Base_tag.influence_list

let influences item =
  List.filter (fun influence -> has_influence influence item) Base_tag.influence_list

let show item =
  let compare_mods
      { modifier = a; fractured = _ }
      { modifier = b; fractured = _ } =
    match a.Mod.domain, b.Mod.domain with
      | Item, Crafted -> -1
      | Crafted, Item -> 1
      | _ ->
          match a.Mod.generation_type, b.Mod.generation_type with
            | Prefix, Suffix -> -1
            | Suffix, Prefix -> 1
            | _ ->
                Id.compare a.id b.id
  in
  let mods = List.sort compare_mods item.mods in
  let rarity = show_rarity item.rarity in
  let influences =
    let check_influence influence =
      if has_influence influence item then
        [ " (" ^ Base_tag.show_influence influence ^ ")" ]
      else
        []
    in
    String.concat "" @@ List.flatten @@ List.map check_influence Base_tag.influence_list
  in
  let split = if item.split then [ "Split" ] else [] in
  "--------\n" ^ Id.show item.base.name ^ " (" ^ rarity ^ ")" ^ influences ^ "\n--------\n" ^
  String.concat "\n" (List.map show_modifier mods @ split) ^ "\n--------"

let make base level rarity =
  {
    base;
    level;
    rarity;
    tags = Id.Set.empty;
    mods = [];
    split = false;
  }

let set_rarity rarity item =
  { item with rarity }

let is_fractured item =
  List.exists (fun { fractured; _ } -> fractured) item.mods

let mod_count item =
  List.length item.mods

(* TODO: jewels
   (domain "abyss_jewel" or ... misc for regular jewels
   or item_class "AbyssJewel" or "Jewel"
   or tag "abyss_jewel" or "jewel") *)
let max_prefix_count item =
  match item.rarity with
    | Normal -> 0
    | Magic -> 1
    | Rare ->
        if Base_item.is_jewel item.base then
          2
        else
          3

let max_suffix_count = max_prefix_count

let prefix_count item =
  List.length (List.filter (fun { modifier; _ } -> Mod.is_prefix modifier) item.mods)

let suffix_count item =
  List.length (List.filter (fun { modifier; _ } -> Mod.is_suffix modifier) item.mods)

let has_a_prefix item =
  List.exists (fun { modifier; _ } -> Mod.is_prefix modifier) item.mods

let has_a_suffix item =
  List.exists (fun { modifier; _ } -> Mod.is_suffix modifier) item.mods

let has_mod_id modifier_id item =
  List.exists (fun { modifier; _ } -> Id.compare modifier.Mod.id modifier_id = 0) item.mods

let has_mod modifier item =
  has_mod_id modifier.Mod.id item

let has_mod_group group item =
  List.exists (fun { modifier; _ } -> Id.compare modifier.Mod.group group = 0) item.mods

let has_veiled_mod item =
  has_mod_group Mod.veiled_prefix_group item ||
  has_mod_group Mod.veiled_suffix_group item

let crafted_mod_count item =
  List.length (List.filter (fun { modifier; _ } -> Mod.is_crafted modifier) item.mods)

let max_crafted_mod_count item =
  if has_mod Mod.(by_id multimod_id) item then
    3
  else
    1

let tags item =
  let add_mod_added_tags acc { modifier; _ } = Id.Set.union acc modifier.Mod.adds_tags in
  List.fold_left add_mod_added_tags (Id.Set.union item.base.tags item.tags) item.mods

let has_tag tag item =
  Id.Set.mem tag (tags item)

(* If [tag] is specified, restrict the mod pool to mods with this tag. *)
let mod_pool ?(fossils = []) ?tag ?(crafted = false) ?only item =
  let item_tags = tags item in
  let prefix_count = prefix_count item in
  let suffix_count = suffix_count item in
  let max_prefix = max_prefix_count item in
  let max_suffix = max_suffix_count item in
  let allow_prefix, allow_suffix =
    match only with
      | None -> true, true
      | Some Mod.Prefix -> true, false
      | Some Mod.Suffix -> false, true
  in
  let not_tags =
    Id.Set.of_list @@ List.flatten [
      if has_mod Mod.(by_id cannot_roll_attack_mods_id) item then
        [ Mod.attack_tag_id ]
      else
        [];
      if has_mod Mod.(by_id cannot_roll_caster_mods_id) item then
        [ Mod.caster_tag_id ]
      else
        [];
    ]
  in
  let can_spawn_mod modifier =
    if
      (crafted && (modifier.Mod.domain <> Crafted)) ||
      (not crafted && (modifier.Mod.domain <> item.base.domain))
    then
      None
    else if
      match tag with
        | None -> false
        | Some tag -> not (Id.Set.mem (Mod.tag_id tag) modifier.tags)
    then
      None
    else if modifier.Mod.required_level > item.level then
      None
    else if Mod.is_prefix modifier && (not allow_prefix || prefix_count >= max_prefix) then
      None
    else if Mod.is_suffix modifier && (not allow_suffix || suffix_count >= max_suffix) then
      None
    else if has_mod_group modifier.Mod.group item then
      None
    else if not (Id.Set.is_empty (Id.Set.inter not_tags modifier.tags)) then
      None
    else
      let matching_tag (tag, weight) =
        if Id.Set.mem tag item_tags then
          Some weight
        else
          None
      in
      let spawn_weights =
        match fossils with
          | [] ->
              modifier.spawn_weights
          | _ :: _ ->
              let apply_combination (id, weight) =
                id, Fossil.apply_combination fossils modifier.tags weight
              in
              List.map apply_combination modifier.spawn_weights
      in
      match List.find_map matching_tag spawn_weights with
        | None ->
            None
        | Some weight when weight <= 0 && not crafted ->
            None
        | Some weight ->
            match List.find_map matching_tag modifier.generation_weights with
              | None ->
                  Some (weight, modifier)
              | Some percent_multiplier ->
                  let weight = weight * percent_multiplier / 100 in
                  if weight <= 0 && not crafted then
                    None
                  else
                    Some (weight, modifier)
  in
  List.filter_map can_spawn_mod !Mod.pool

let add_mod_force ?(fractured = false) modifier item =
  { item with mods = { modifier = Mod.roll_modifier modifier; fractured } :: item.mods }

let add_mod ?(fractured = false) modifier item =
  if fractured && is_fractured item then
    fail "cannot add a second fractured modifier";
  if Mod.is_prefix modifier && prefix_count item >= max_prefix_count item then
    fail "cannot add another prefix";
  if Mod.is_suffix modifier && suffix_count item >= max_suffix_count item then
    fail "cannot add another suffix";
  if Mod.is_crafted modifier then (
    if crafted_mod_count item >= max_crafted_mod_count item then
      fail "item cannot have another crafted mod";
  )
  else (
    if has_mod_group modifier.group item then
      fail "item already has a mod for this group";
  );
  add_mod_force ~fractured modifier item

let spawn_random_mod ?(fail_if_impossible = true) ?fossils ?tag item =
  match random_from_pool (mod_pool ?fossils ?tag item) with
    | None ->
        if fail_if_impossible then
          match tag with
            | None ->
                fail "item cannot spawn any mod"
            | Some tag ->
                fail "item cannot spawn any mod with tag %s" (Id.show (Mod.tag_id tag))
        else
          item
    | Some modifier ->
        add_mod_force modifier item

type meta =
  {
    prefixes_cannot_be_changed: bool;
    suffixes_cannot_be_changed: bool;
    cannot_roll_attack_mods: bool;
    cannot_roll_caster_mods: bool;
  }

let meta ~respect_cannot_be_changed ~respect_cannot_roll item =
  let prefixes_cannot_be_changed =
    respect_cannot_be_changed &&
    has_mod Mod.(by_id prefixes_cannot_be_changed_id) item
  in
  let suffixes_cannot_be_changed =
    respect_cannot_be_changed &&
    has_mod Mod.(by_id suffixes_cannot_be_changed_id) item
  in
  let cannot_roll_attack_mods =
    respect_cannot_roll &&
    has_mod Mod.(by_id cannot_roll_attack_mods_id) item
  in
  let cannot_roll_caster_mods =
    respect_cannot_roll &&
    has_mod Mod.(by_id cannot_roll_caster_mods_id) item
  in
  {
    prefixes_cannot_be_changed;
    suffixes_cannot_be_changed;
    cannot_roll_attack_mods;
    cannot_roll_caster_mods;
  }

let can_be_removed meta { modifier; fractured } =
  not fractured &&
  not (meta.prefixes_cannot_be_changed && Mod.is_prefix modifier) &&
  not (meta.suffixes_cannot_be_changed && Mod.is_suffix modifier) &&
  not (meta.cannot_roll_attack_mods && Mod.is_attack modifier) &&
  not (meta.cannot_roll_caster_mods && Mod.is_caster modifier)

let remove_random_mod ?without_tag ~respect_cannot_be_changed ~respect_cannot_roll item =
  let meta = meta ~respect_cannot_be_changed ~respect_cannot_roll item in
  let removable_mods = List.filter (can_be_removed meta) item.mods in
  let removable_mods =
    match without_tag with
      | None ->
          removable_mods
      | Some tag ->
          let tag_id = Mod.tag_id tag in
          let can_be_removed { modifier; _ } = not (Id.Set.mem tag_id modifier.tags) in
          List.filter can_be_removed item.mods
  in
  let removable_mod_count = List.length removable_mods in
  if removable_mod_count <= 0 then
    fail "item has no removable mod";
  let mod_to_remove = (Array.of_list removable_mods).(Random.int removable_mod_count) in
  let mods =
    List.filter
      (fun { modifier; _ } -> Id.compare modifier.Mod.id mod_to_remove.modifier.id <> 0)
      item.mods
  in
  { item with mods }

let remove_crafted_mods item =
  let mods = List.filter (fun { modifier; _ } -> not (Mod.is_crafted modifier)) item.mods in
  { item with mods }

let remove_all_mods ~respect_cannot_be_changed ~respect_cannot_roll item =
  let meta = meta ~respect_cannot_be_changed ~respect_cannot_roll item in
  let mods = List.filter (fun modifier -> not (can_be_removed meta modifier)) item.mods in
  { item with mods }

let remove_all_prefixes item =
  let mods =
    List.filter
      (fun { modifier; fractured } -> fractured || not (Mod.is_prefix modifier))
      item.mods
  in
  { item with mods }

let remove_all_suffixes item =
  let mods =
    List.filter
      (fun { modifier; fractured } -> fractured || not (Mod.is_suffix modifier))
      item.mods
  in
  { item with mods }

let set_to_lowest_possible_rarity item =
  let p = prefix_count item in
  let s = suffix_count item in
  let rarity =
    if p > 1 || s > 1 then Rare else
    if p > 0 || s > 0 then Magic else
      Normal
  in
  { item with rarity }

let spawn_additional_random_mods ?fossils item =
  let spawn_random_mod = spawn_random_mod ~fail_if_impossible: false ?fossils in
  (* Tested a small sample, counting how many items had 4, 5, 6 mods after a chaos. *)
  (* TODO: better sample *)
  let w4 = 56 in
  let w5 = 22 in
  let w6 = 3 in
  let add_from_4 item =
    let i = Random.int (w4 + w5 + w6) in
    if i < w4 then
      item
    else if i < w4 + w5 then
      spawn_random_mod item
    else
      spawn_random_mod (spawn_random_mod item)
  in
  match mod_count item with
    | 0 ->
        item |> spawn_random_mod |> spawn_random_mod |> spawn_random_mod |> spawn_random_mod
        |> add_from_4
    | 1 ->
        item |> spawn_random_mod |> spawn_random_mod |> spawn_random_mod
        |> add_from_4
    | 2 ->
        item |> spawn_random_mod |> spawn_random_mod
        |> add_from_4
    | 3 ->
        item |> spawn_random_mod
        |> add_from_4
    | 4 ->
        item
        |> add_from_4
    | 5 ->
        let i = Random.int (w4 + w5 + w6) in
        if i < w4 + w5 then
          item
        else
          spawn_random_mod item
    | _ ->
        item

let reforge_magic item =
  let item =
    remove_all_mods item
      ~respect_cannot_be_changed: true
      ~respect_cannot_roll: false
  in
  let item = spawn_random_mod item in
  if Random.bool () then spawn_random_mod ~fail_if_impossible: false item else item

(* If [modifier] is specified, [tag] cannot be specified; [modifier] is added first. *)
(* [tag] is only applied to the first added mod. *)
let reforge_rare ?(respect_cannot_be_changed = true) ?fossils ?tag ?modifier item =
  let item =
    remove_all_mods item
      ~respect_cannot_be_changed
      ~respect_cannot_roll: false
  in
  let item =
    match tag, modifier with
      | _, None ->
          spawn_random_mod ?fossils ?tag item
      | None, Some modifier ->
          add_mod modifier item
      | Some _, Some _ ->
          invalid_arg "Item.reforge_rare cannot take both ?tag and ?modifier"
  in
  spawn_additional_random_mods ?fossils item

let reforge_rare_suffixes item =
  let item = remove_all_suffixes item in
  let item = spawn_random_mod ~fail_if_impossible: false item in
  spawn_additional_random_mods item

let reforge_rare_prefixes item =
  let item = remove_all_prefixes item in
  let item = spawn_random_mod ~fail_if_impossible: false item in
  spawn_additional_random_mods item

let add_influence influence item =
  match Base_tag.get_influence_tag_for_tags item.base.tags influence with
    | None ->
        fail "cannot add an influence to this item type"
    | Some influence_tag ->
        { item with tags = Id.Set.add influence_tag item.tags }

let is_influence_mod influence_tag modifier =
  let tag_with_weight (mod_tag, weight) =
    weight > 0 &&
    Id.compare mod_tag influence_tag = 0
  in
  List.exists tag_with_weight modifier.Mod.spawn_weights

let spawn_random_influence_mod influence item =
  match Base_tag.get_influence_tag_for_tags item.base.tags influence with
    | None ->
        fail "cannot add an influence to this item type"
    | Some influence_tag ->
        let item = { item with tags = Id.Set.add influence_tag item.tags } in
        let pool = mod_pool item in
        let pool =
          let is_influence_mod (_, modifier) = is_influence_mod influence_tag modifier in
          List.filter is_influence_mod pool
        in
        match random_from_pool pool with
          | None ->
              fail "item cannot spawn any mod for this influence"
          | Some modifier ->
              add_mod_force modifier item

let add_random_veiled_mod item =
  match prefix_count item < 3, suffix_count item < 3 with
    | false, false ->
        fail "item has no space for another mod"
    | true, false ->
        add_mod Mod.(by_id veiled_prefix_id) item
    | false, true ->
        add_mod Mod.(by_id veiled_suffix_id) item
    | true, true ->
        (* TODO: do the weights apply? *)
        if Random.bool () then
          add_mod Mod.(by_id veiled_prefix_id) item
        else
          add_mod Mod.(by_id veiled_suffix_id) item

let remove_random_mod_add_veiled_mod item =
  if has_veiled_mod item then
    fail "item already has a veiled modifier"
  else
    let item =
      remove_random_mod item
        ~respect_cannot_be_changed: true
        ~respect_cannot_roll: false
    in
    add_random_veiled_mod item

(* [tag] is only applied to the first added mod. *)
let reforge_rare_with_veiled_mod item =
  let item =
    remove_all_mods item
      ~respect_cannot_be_changed: true
      ~respect_cannot_roll: false
  in
  let item = add_random_veiled_mod item in
  spawn_additional_random_mods item

let is_prefix_or_suffix { modifier; _ } = Mod.is_prefix_or_suffix modifier

let split item =
  if item.split then fail "item is already split";
  let mods_to_split, non_splittable_mods = List.partition is_prefix_or_suffix item.mods in
  let pool = Pool.create_from_list mods_to_split in
  (* Both resulting items will have at least one mod. *)
  let mandatory_mod1, mandatory_mod2 =
    let mod1 = Pool.pick pool in
    let mod2 = Pool.pick pool in
    match mod1, mod2 with
      | Some mod1, Some mod2 -> mod1, mod2
      | None, _ | _, None -> fail "cannot split an item with less than two modifiers"
  in
  let other_mods1, other_mods2 =
    List.partition (fun _ -> Random.bool ()) (Pool.to_list pool)
  in
  let mods1 = mandatory_mod1 :: other_mods1 @ non_splittable_mods in
  let mods2 = mandatory_mod2 :: other_mods2 @ non_splittable_mods in
  set_to_lowest_possible_rarity { item with mods = mods1; split = true },
  set_to_lowest_possible_rarity { item with mods = mods2; split = true }

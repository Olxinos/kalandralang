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
    influence: Influence.t;
  }

let get_min_stat_value (stat_id : Id.t) (item : t) : int =
  let accumulate acc modifier =
    Mod.get_min_stat_value ~acc: acc modifier.modifier stat_id
  in
    List.fold_left accumulate 0 item.mods

let get_max_stat_value (stat_id : Id.t) (item : t) : int =
  let accumulate acc modifier =
    Mod.get_max_stat_value ~acc: acc modifier.modifier stat_id
  in
    List.fold_left accumulate 0 item.mods

let pp { base; level; tags; rarity; mods; split; influence } =
  Pretext.OCaml.record [
    "base", Base_item.pp base;
    "level", Pretext.OCaml.int level;
    "tags", Id.Set.pp tags;
    "rarity", pp_rarity rarity;
    "mods", Pretext.OCaml.list pp_modifier mods;
    "split", Pretext.OCaml.bool split;
    "influence", Influence.pp influence;
  ]

let show_modifier { modifier; fractured } =
  Mod.show ~fractured With_random_values modifier

let show item =
  let compare_mods
      { modifier = a; fractured = _ }
      { modifier = b; fractured = _ } =
    match a.Mod.domain, b.Mod.domain with
      | Item, Crafted -> -1
      | Crafted, Item -> 1
      | _ ->
          let int_of_generation_type = function
            | Mod.Exarch_implicit _ -> 0
            | Mod.Eater_implicit _ -> 1
            | Mod.Prefix -> 2
            | Mod.Suffix -> 3
          in
          let c =
            Int.compare
              (int_of_generation_type a.Mod.generation_type)
              (int_of_generation_type b.Mod.generation_type)
          in
          if c <> 0 then c else Id.compare a.id b.id
  in
  let mods = List.sort compare_mods item.mods in
  let rarity = show_rarity item.rarity in
  let influence =
    match item.influence with
      | Not_influenced ->
          ""
      | Fractured ->
          " (Fractured)"
      | Synthesized ->
          " (Synthesized)"
      | SEC sec ->
          " (" ^ Influence.show_sec sec ^ ")"
      | SEC_pair (sec1, sec2) ->
          " (" ^ Influence.show_sec sec1 ^ " / " ^ Influence.show_sec sec2 ^ ")"
      | Exarch ->
          " (Exarch)"
      | Eater ->
          " (Eater)"
      | Exarch_and_eater ->
          " (Exarch / Eater)"
  in
  let split = if item.split then [ "Split" ] else [] in
  "--------\n" ^ Id.show item.base.name ^ " (" ^ rarity ^ ")" ^ influence ^ "\n--------\n" ^
  String.concat "\n" (List.map show_modifier mods @ split) ^ "\n--------"

let set_rarity rarity item =
  { item with rarity }

let is_fractured item =
  List.exists (fun { fractured; _ } -> fractured) item.mods

let is_prefix_or_suffix { modifier; _ } =
  Mod.is_prefix_or_suffix modifier

let prefix_and_suffix_count item =
  List.length (List.filter is_prefix_or_suffix item.mods)

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

type only =
  | Prefixes_and_suffixes
  | Prefixes
  | Suffixes
  | Eater_implicits of Mod.eldritch_tier
  | Exarch_implicits of Mod.eldritch_tier

(* If [tag] is specified, restrict the mod pool to mods with this tag. *)
let mod_pool ?(fossils = []) ?tag ?(crafted = false) ?(only = Prefixes_and_suffixes) item =
  let item_tags = tags item in
  let prefix_count = prefix_count item in
  let suffix_count = suffix_count item in
  let max_prefix = max_prefix_count item in
  let max_suffix = max_suffix_count item in
  let allow_prefix, allow_suffix =
    match only with
      | Prefixes_and_suffixes -> true, true
      | Prefixes -> true, false
      | Suffixes -> false, true
      | Eater_implicits _ | Exarch_implicits _ -> false, false
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
    let can_spawn_this_generation_type =
      (* Note: max prefix/suffix count is handled below, not here *)
      match only, modifier.Mod.generation_type with
        | Prefixes_and_suffixes, (Prefix | Suffix)
        | Prefixes, Prefix
        | Suffixes, Suffix ->
            true
        | Exarch_implicits only_tier, Exarch_implicit mod_tier
        | Eater_implicits only_tier, Eater_implicit mod_tier ->
            only_tier = mod_tier
        | _ ->
            false
    in
    let already_has_mod_group =
      match modifier.Mod.generation_type with
        | Exarch_implicit _
        | Eater_implicit _ ->
            (* My experiment on one mod (unveiling move speed + onslaught on boots)
               seems to show that implicits and prefixes/suffixes are independent,
               but this needs to be experimented on more. *)
            (* Since we can only have one of each eldritch implicits,
               and [mod_pool] is used to *replace* mods when used with eldritch implicits,
               we allow all eldritch implicits to be in the mod pool. *)
            false
        | _ ->
            has_mod_group modifier.Mod.group item
    in
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
    else if not can_spawn_this_generation_type then
      None
    else if already_has_mod_group then
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
  { item with mods = { modifier; fractured } :: item.mods }

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

let spawn_random_mod ?(fail_if_impossible = true) ?fossils ?tag ?only item =
  match random_from_pool (mod_pool ?fossils ?tag ?only item) with
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

let meta
    ?(force_prefixes_cannot_be_changed = false)
    ?(force_suffixes_cannot_be_changed = false)
    ~respect_cannot_be_changed
    ~respect_cannot_roll
    item =
  let prefixes_cannot_be_changed =
    force_prefixes_cannot_be_changed || (
      respect_cannot_be_changed &&
      has_mod Mod.(by_id prefixes_cannot_be_changed_id) item
    )
  in
  let suffixes_cannot_be_changed =
    force_suffixes_cannot_be_changed || (
      respect_cannot_be_changed &&
      has_mod Mod.(by_id suffixes_cannot_be_changed_id) item
    )
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
  Mod.is_prefix_or_suffix modifier &&
  not fractured &&
  not (meta.prefixes_cannot_be_changed && Mod.is_prefix modifier) &&
  not (meta.suffixes_cannot_be_changed && Mod.is_suffix modifier) &&
  not (meta.cannot_roll_attack_mods && Mod.is_attack modifier) &&
  not (meta.cannot_roll_caster_mods && Mod.is_caster modifier)

let remove_random_mod
    ?without_tag
    ?force_prefixes_cannot_be_changed
    ?force_suffixes_cannot_be_changed
    ~respect_cannot_be_changed
    ~respect_cannot_roll
    item =
  let meta =
    meta
      ?force_prefixes_cannot_be_changed
      ?force_suffixes_cannot_be_changed
      ~respect_cannot_be_changed
      ~respect_cannot_roll
      item
  in
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

let spawn_additional_random_mods ?fossils ?only item =
  let spawn_random_mod = spawn_random_mod ~fail_if_impossible: false ?fossils ?only in
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
  match prefix_and_suffix_count item with
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

let reforge_rare_suffixes ?(can_add_prefixes = true) item =
  let item = remove_all_suffixes item in
  let only = if can_add_prefixes then None else Some Suffixes in
  let item = spawn_random_mod ~fail_if_impossible: false ?only item in
  spawn_additional_random_mods ?only item

let reforge_rare_prefixes ?(can_add_suffixes = true) item =
  let item = remove_all_prefixes item in
  let only = if can_add_suffixes then None else Some Prefixes in
  let item = spawn_random_mod ~fail_if_impossible: false ?only item in
  spawn_additional_random_mods ?only item

let add_sec_influence_tag influence item =
  match Base_tag.get_influence_tag_for_tags item.base.tags influence with
    | None ->
        fail "cannot add an influence to this item type"
    | Some influence_tag ->
        { item with tags = Id.Set.add influence_tag item.tags }

let add_influence influence item =
  let item = { item with influence = Influence.add item.influence influence } in
  match influence with
    | SEC sec ->
        item |> add_sec_influence_tag sec
    | SEC_pair (sec1, sec2) ->
        item |> add_sec_influence_tag sec1 |> add_sec_influence_tag sec2
    | Not_influenced | Fractured | Synthesized | Exarch | Eater | Exarch_and_eater ->
        item

let make base level rarity influence =
  {
    base;
    level;
    rarity;
    tags = Id.Set.empty;
    mods = [];
    split = false;
    influence = Not_influenced;
  }
  |> add_influence influence

let is_influence_mod influence_tag modifier =
  let tag_with_weight (mod_tag, weight) =
    weight > 0 &&
    Id.compare mod_tag influence_tag = 0
  in
  List.exists tag_with_weight modifier.Mod.spawn_weights

let spawn_random_sec_influence_mod influence item =
  match Base_tag.get_influence_tag_for_tags item.base.tags influence with
    | None ->
        fail "cannot add influence to this item type"
    | Some influence_tag ->
        let item = add_influence (SEC influence) item in
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

let is_exarch_implicit_or_not_an_implicit { modifier; _ } =
  Mod.is_exarch_implicit modifier ||
  not (Mod.is_implicit modifier)

let is_eater_implicit_or_not_an_implicit { modifier; _ } =
  Mod.is_eater_implicit modifier ||
  not (Mod.is_implicit modifier)

let remove_all_implicits_except_exarch item =
  let mods = List.filter is_exarch_implicit_or_not_an_implicit item.mods in
  { item with mods }

let remove_all_implicits_except_eater item =
  let mods = List.filter is_eater_implicit_or_not_an_implicit item.mods in
  { item with mods }

let spawn_random_exarch_implicit tier item =
  spawn_random_mod ~only: (Exarch_implicits tier) item

let spawn_random_eater_implicit tier item =
  spawn_random_mod ~only: (Eater_implicits tier) item

let apply_eldritch_ichor tier item =
  item
  |> add_influence Eater
  |> remove_all_implicits_except_exarch
  |> spawn_random_eater_implicit tier

let apply_eldritch_ember tier item =
  item
  |> add_influence Exarch
  |> remove_all_implicits_except_eater
  |> spawn_random_exarch_implicit tier

type dominant_eldritch =
  | Exarch
  | Eater

let get_dominant_eldritch item =
  let exarch_tier = ref None in
  let eater_tier = ref None in
  let update_tiers { modifier; _ } =
    match modifier.generation_type with
      | Exarch_implicit tier -> exarch_tier := Some tier
      | Eater_implicit tier -> eater_tier := Some tier
      | _ -> ()
  in
  List.iter update_tiers item.mods;
  match !exarch_tier, !eater_tier with
    | None, None -> None
    | None, Some _ -> Some Eater
    | Some _, None -> Some Exarch
    | Some exarch_tier, Some eater_tier ->
        let c = Mod.compare_eldritch_tiers exarch_tier eater_tier in
        if c > 0 then Some Exarch else
        if c < 0 then Some Eater else
          None

let with_dominance item f =
  match get_dominant_eldritch item with
    | None ->
        fail "neither the Searing Exarch nor the Eater of Worlds is dominant"
    | Some dominance ->
        f dominance

let apply_eldritch_annul item =
  with_dominance item @@ function
  | Exarch ->
      remove_random_mod
        ~force_suffixes_cannot_be_changed: true
        ~respect_cannot_be_changed: true
        ~respect_cannot_roll: true
        item
  | Eater ->
      remove_random_mod
        ~force_prefixes_cannot_be_changed: true
        ~respect_cannot_be_changed: true
        ~respect_cannot_roll: true
        item

let apply_eldritch_exalt item =
  with_dominance item @@ function
  | Exarch -> spawn_random_mod ~only: Prefixes item
  | Eater -> spawn_random_mod ~only: Suffixes item

let apply_eldritch_chaos item =
  with_dominance item @@ function
  | Exarch -> reforge_rare_prefixes ~can_add_suffixes: false item
  | Eater -> reforge_rare_suffixes ~can_add_prefixes: false item

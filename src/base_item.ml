open Misc

type domain =
  | Item
  | Crafted
  | Veiled
  | Misc (* tree-only jewels *)
  | Abyss_jewel

let show_domain = function
  | Item -> "Item"
  | Crafted -> "Crafted"
  | Veiled -> "Veiled"
  | Misc -> "Misc"
  | Abyss_jewel -> "Abyss_jewel"

let pp_domain domain =
  Pretext.OCaml.variant (show_domain domain) []

let as_domain json =
  match JSON.as_string json with
    | "item" ->
        Some Item
    | "crafted" ->
        Some Crafted
    | "veiled" ->
        Some Veiled
    | "misc" ->
        Some Misc
    | "abyss_jewel" ->
        Some Abyss_jewel
    | _ ->
        None

type t =
  {
    id: Id.t;
    domain: domain;
    item_class: Id.t;
    name: Id.t;
    (* [tags] restricts the mod pool. *)
    tags: Id.Set.t;
    properties: int Id.Map.t;
  }

let pp { id; domain; item_class; name; tags; properties } =
  Pretext.OCaml.record [
    "id", Id.pp id;
    "domain", pp_domain domain;
    "item_class", Id.pp item_class;
    "name", Id.pp name;
    "tags", Id.Set.pp tags;
    "properties", Id.pp_int_map properties
  ]

let id_map = ref Id.Map.empty

type data = t Id.Map.t
let export (): data = !id_map
let import (x: data) = id_map := x

let load filename =
  let add_entry (id, values) =
    let id = Id.make id in
    let domain = ref None in
    let item_class = ref Id.empty in
    let name = ref Id.empty in
    let tags = ref Id.Set.empty in
    let properties = ref Id.Map.empty in
    let handle_value (field, value) =
      match field with
        | "domain" ->
            domain := as_domain value
        | "item_class" ->
            item_class := JSON.as_id value
        | "name" ->
            name := JSON.as_id value
        | "tags" ->
            tags := JSON.as_array value |> List.map JSON.as_id |> Id.Set.of_list
        | "properties" ->
            let f (label, value) =
              match JSON.as_int_opt value with
                | Some x -> Some (Id.make label, x)
                | _ -> None
            in
              properties := JSON.as_object value |> List.filter_map f |> List.to_seq |> Id.Map.of_seq
        | _ ->
            ()
    in
    List.iter handle_value (JSON.as_object values);
    match !domain with
      | None | Some Crafted | Some Veiled ->
          ()
      | Some (Item | Misc | Abyss_jewel as domain) ->
          let base_item =
            {
              id;
              domain;
              item_class = !item_class;
              name = !name;
              tags = !tags;
              properties = !properties;
            }
          in
          match Id.Map.find_opt id !id_map with
            | Some _ ->
                fail "%s: two items with id %s" filename (Id.show id)
            | None ->
                id_map := Id.Map.add id base_item !id_map
  in
  List.iter add_entry JSON.(parse_file filename |> as_object)

let get_property base_item (property_id : Id.t) : int =
  match Id.Map.find_opt property_id base_item.properties with
    | Some x -> x
    | None -> fail "no '%S' property in base item %S" (Id.show property_id) (Id.show base_item.id)

let attack_time_in_milliseconds base_item =
  get_property base_item (Id.make "attack_time")

let min_physical_damage base_item =
  get_property base_item (Id.make "physical_damage_min")

let max_physical_damage base_item =
  get_property base_item (Id.make "physical_damage_max")

let by_id id =
  match Id.Map.find_opt id !id_map with
    | None ->
        fail "no base item with id %S" (Id.show id)
    | Some x ->
        x

let jewel_class = Id.make "Jewel"
let abyss_jewel_class = Id.make "AbyssJewel"

let is_jewel { item_class; _ } =
  Id.compare item_class jewel_class = 0 ||
  Id.compare item_class abyss_jewel_class = 0

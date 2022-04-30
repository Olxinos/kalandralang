(* USES uri *)
(* USES tls *)
(* USES cohttp-lwt-unix *)
(* USES lwt *)

open Misc

let (let*) = Lwt.bind
let return = Lwt.return

let get ?(headers = Cohttp.Header.init ()) uri =
  Lwt_main.run @@
  let* (response, response_body) = Cohttp_lwt_unix.Client.get ~headers uri in
  let* body = Cohttp_lwt.Body.to_string response_body in
  let code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  match code with
    (* OK: Succuessful get request with body content *)
    | 200 ->
        return (response, (Some body))
    (* Not-modified: Succuessful get request with no body content, content hasnot been modified *)
    | 304 ->
        return (response, None)
    (* else report error *)
    | _ ->
        echo "failed to fetch %s: %s - %s"
          (Uri.to_string uri)
          (Cohttp.Code.string_of_status response.status)
          body;
        return (response, None)

let get_json ?(headers = Cohttp.Header.init ()) origin = function uri ->
  let (_, response_body) = get ~headers uri in
    match response_body with
      | Some response_body -> Some (JSON.parse ~origin: origin response_body)
      | _ -> None

let download ?(headers = Cohttp.Header.init ()) filepath = function uri ->
  let (response, response_body) = get ~headers uri in
    match response_body with
    | Some resp -> 
        let oc = open_out filepath in
        Printf.fprintf oc "%s" resp;
        echo "Wrote Data to Disk %d" (String.length resp);
        close_out oc;
        (response, response_body)
    | _ -> 
      (response, response_body)

let get_header header = function (response, _) ->
  let headers = response |> Cohttp.Response.headers in
  let tag = Cohttp.Header.get headers header in
  tag

let header_create = Cohttp.Header.init

let header_add name value = function h ->
  Cohttp.Header.add h name value
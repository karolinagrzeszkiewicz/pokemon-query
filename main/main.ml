open Core
open Async 
open Yojson
open JsonParsing
open Cohttp_async

(* User input *)

let ask_for_input () = 
  let stdin = Lazy.force Reader.stdin in
  print_endline "> Enter a Pokemon name: ";
  Reader.read_line stdin
  >>| function
  | `Eof -> "" (* None *)
  | `Ok s -> s (* Some s *)

(* API calls *)

let get_uri_api1 name = (name, Uri.of_string ("https://pokeapi.co/api/v2/pokemon-species/"^name))

let get_uri_api2 name = (name, Uri.of_string ("https://pokeapi.co/api/v2/pokemon/"^name))

let get_reqBody name uri = (* val get_reqBody : 'a -> Uri.t -> ('a * string option) Deferred.t *)
  try_with 
    (fun () -> (Client.call `GET uri >>= fun (_, body) ->
    body |> Cohttp_async.Body.to_string >>| fun body -> body))
  >>| function
  | Ok "Not Found" | Error _ -> (name, None)
  | Ok str -> (name, Some str)

(* the program *)

let run () =
  ask_for_input () 
  >>| (fun name -> get_uri_api1 name)
  >>= (fun (name, uri) -> get_reqBody name uri) (* (string * string option) Deferred.t *)
  >>| (fun pair -> name_printer pair)
  >>| (fun pair -> is_legendary_printer pair)
  >>| (fun pair -> habitat_printer pair)
  >>| (fun pair -> description_printer pair)
  >>| (fun (name, _) -> get_uri_api2 name)
  >>= (fun (name, uri) -> get_reqBody name uri)
  >>| (fun pair -> height_printer pair)
  >>| (fun pair -> types_printer pair)
  >>= (fun _ -> exit 0)


let () = 
  let _ = run () in
  (*don't_wait_for (exit 0);*)
  never_returns (Scheduler.go ())






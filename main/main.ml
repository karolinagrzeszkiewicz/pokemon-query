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
  | `Eof -> ""
  | `Ok s -> s



(* API calls *)

let get_reqBody_api1 name =
  let uri = Uri.of_string ("https://pokeapi.co/api/v2/pokemon-species/"^name) in
  Client.call `GET uri >>= fun (_, body) ->
  body |> Cohttp_async.Body.to_string >>| fun body -> body

let get_reqBody_api2 name =
  let uri = Uri.of_string ("https://pokeapi.co/api/v2/pokemon/"^name) in
  Client.call `GET uri >>= fun (_, body) ->
  body |> Cohttp_async.Body.to_string >>| fun body -> body


let run () = 
  ((ask_for_input () >>= fun name -> get_reqBody_api1 name) 
  |> get_habitat) >>| 
  fun opt_str -> Option.iter opt_str (fun s ->  print_endline ("> "^s^"\n")


let () = 
  let _ =  run () in
  (*don't_wait_for (exit 0);*)
  never_returns (Scheduler.go ())





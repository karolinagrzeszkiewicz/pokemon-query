open Core
open Async 
open Yojson
open JsonParsing
open Cohttp_async


(* API calls *)

let get_reqBody_api1 name =
  let uri = Uri.of_string ("https://pokeapi.co/api/v2/pokemon-species/"^name) in
  Client.call `GET uri >>= fun (_, body) ->
  body |> Cohttp_async.Body.to_string >>| fun body -> body

let get_reqBody_api2 name =
  let uri = Uri.of_string ("https://pokeapi.co/api/v2/pokemon/"^name) in
  Client.call `GET uri >>= fun (_, body) ->
  body |> Cohttp_async.Body.to_string >>| fun body -> body





(*let () = 
  let _ =  ... in
  (*don't_wait_for (exit 0);*)
  never_returns (Scheduler.go ()) *)





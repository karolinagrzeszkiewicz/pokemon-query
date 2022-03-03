open Core
open Async 
open Yojson
open JsonHelper
open JsonParsing
open Printers
open Cohttp_async


(* User input *)

let ask_for_input (() : unit) : string Deferred.t = 
  let stdin = Lazy.force Reader.stdin in
  print_endline "> Enter a Pokemon name: ";
  Reader.read_line stdin
  >>| function
  | `Eof -> "" 
  | `Ok s -> s 

(* API calls *)

let get_uri_api1 (name : string) : string * Uri.t = (name, Uri.of_string ("https://pokeapi.co/api/v2/pokemon-species/"^name))

let get_uri_api2 (name : string) : string * Uri.t = (name, Uri.of_string ("https://pokeapi.co/api/v2/pokemon/"^name))

let get_reqBody (name : string) (uri : Uri.t) : (string * string option) Deferred.t = 
  try_with 
    (fun () -> (Client.call `GET uri >>= fun (_, body) ->
    body |> Cohttp_async.Body.to_string >>| fun body -> body))
  >>| function
  | Ok "Not Found" | Error _ -> (name, None)
  | Ok str -> (name, Some str)


(* the program *)

let signal_invalid (json_opt : string option) : unit = 
  if Option.is_empty json_opt 
  then print_endline "Invalid Pokemon name!"


let run (() : unit) : unit Deferred.t =
  ask_for_input () 
  >>| (fun name -> get_uri_api1 name)
  >>= (fun (name, uri) -> get_reqBody name uri) 
  >>| (fun pair -> NamePrinter.print pair)
  >>| (fun pair -> IsLegendaryPrinter.print pair)
  >>| (fun pair -> HabitatPrinter.print pair)
  >>| (fun pair -> DescriptionPrinter.print pair)
  >>| (fun (name, _) -> get_uri_api2 name)
  >>= (fun (name, uri) -> get_reqBody name uri)
  >>| (fun pair -> HeightPrinter.print pair)
  >>| (fun pair -> TypesPrinter.print pair)
  >>| (fun (name, json_opt) -> signal_invalid json_opt)
  >>= (fun _ -> exit 0)


let () = 
  let _ = run () in
  never_returns (Scheduler.go ())






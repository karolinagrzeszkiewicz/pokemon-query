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



(* VERSION 2: uncomment the section below to test it *)
(* note: still needs some work *)

(*

let print_info_api1 name = 
  return (get_uri_api1 name)
  >>= (fun (name, uri) -> get_reqBody name uri) 
  >>| (fun pair -> NamePrinter.print pair)
  >>| (fun pair -> IsLegendaryPrinter.print pair)
  >>| (fun pair -> HabitatPrinter.print pair)
  >>| (fun pair -> DescriptionPrinter.print pair)
  >>| (fun (name, json_opt) -> signal_invalid json_opt)

let print_info_api2 name = 
  return (get_uri_api2 name)
  >>= (fun (name, uri) -> get_reqBody name uri)
  >>| (fun pair -> HeightPrinter.print pair)
  >>| (fun pair -> TypesPrinter.print pair)


let run (() : unit) : unit Deferred.t =
  ask_for_input () >>= 
  (fun name -> (print_info_api1 name >>= fun _ -> print_info_api2 name))
  >>= (fun _ -> exit 0)
*)

(* PARALLELISED VERSION: uncomment the section below to test it *)
(* note: still needs some work e.g. in handling the output from printers, and incorrect user input *)

(*
let printer (name : string) (key : string) (json : string option) : string * string option =
  begin match key with
  | "name" -> NamePrinter.print (name, json)
  | "is_legendary" -> IsLegendaryPrinter.print (name, json)
  | "habitat" -> HabitatPrinter.print (name, json)
  | "description" -> DescriptionPrinter.print (name, json)
  | "height" -> HeightPrinter.print (name, json)
  | "types" -> TypesPrinter.print (name, json)
  | _ -> (name, json)
  end 

(* Deferred.all: list of deferred to deferred of list *)
let print_info (name : string) (json : string option) (keys : string list) : (string * string option) list Deferred.t = 
  Deferred.all (List.map keys (fun key -> return (printer name key json)))

let call_api_and_print (name : string) : (string * string option) list list Deferred.t = 
  Deferred.all (List.mapi [get_uri_api1 name; get_uri_api2 name] (fun i (name, body) -> (get_reqBody name body 
  >>= fun (name, json) -> if i = 0 then print_info name json ["name";"is_legendary";"habitat";"description"]
  else print_info name json ["height"; "types"]))) 

let run (() : unit) : unit Deferred.t =
  ask_for_input () >>= 
  (fun name -> call_api_and_print name)
  >>= (fun _ -> exit 0)

let () = 
  let _ = run () in
  never_returns (Scheduler.go ())
*)

  






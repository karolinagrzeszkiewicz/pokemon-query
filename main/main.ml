
open Core
open Async 
open Yojson
(*
open Uri 
open Cohttp
open Async_ssl
*)
open Cohttp_async


let reqBody =
  let uri = Uri.of_string "https://pokeapi.co/api/v2/pokemon-species/mewtwo" in
  Client.call `GET uri >>= fun (_, body) ->
  body |> Cohttp_async.Body.to_string >>| fun body -> body

let print_async str = 
  str >>| fun s -> print_endline s

  (*
let () = 
  let _ = print_async reqBody in
  never_returns (Scheduler.go ()) 
  *)


(* Extract a given field from the query result *)
let get_field_from_json field json =
  match Yojson.Safe.from_string json with
  | `Assoc kv_list -> (
      let find key =
        match List.Assoc.find ~equal:String.equal kv_list key with
        | None | Some (`String "") -> None
        | Some s -> Some (Yojson.Safe.to_string s)
      in
      match find field with Some _ as x -> x | None -> None)
  | _ -> None


let get_color = 
  (reqBody >>| fun str -> get_field_from_json "color" str)
  >>| (fun opt_str -> Option.iter opt_str (fun str -> print_endline str))

let () = 
  let _ =  get_color in
  never_returns (Scheduler.go ()) 


module PokemonQuery = 
  struct 

    (*type pokemon_name = Name of string * string
    type pokemon_description = Description of string * string 
    type pokemon_height *)

    type query = string * string

    type pokemon = {
      name : string;
      description : string;
      height : int;
      types : string list;
      habitat : string;
      is_legendary : bool 
    }

      
    (* make pokemon with the following expressions replaced by relevant values:

    {
      name = ...;
      description = ...;
      height = ...;
      types = ...;
      habitat = ...;
      is_legendary = ...
    }
    
    *)



    end 


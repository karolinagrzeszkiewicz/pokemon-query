
open Core
open Async 
open Yojson
(*
open Uri 
open Cohttp
open Async_ssl
*)
open Cohttp_async

(* for testing *)
let reqBody =
  let uri = Uri.of_string ("https://pokeapi.co/api/v2/pokemon-species/mewtwo") in
  Client.call `GET uri >>= fun (_, body) ->
  body |> Cohttp_async.Body.to_string >>| fun body -> body

let reqBody2 =
  let uri = Uri.of_string ("https://pokeapi.co/api/v2/pokemon/mewtwo") in
  Client.call `GET uri >>= fun (_, body) ->
  body |> Cohttp_async.Body.to_string >>| fun body -> body

let ditto1 =
  let uri = Uri.of_string ("https://pokeapi.co/api/v2/pokemon-species/ditto") in
  Client.call `GET uri >>= fun (_, body) ->
  body |> Cohttp_async.Body.to_string >>| fun body -> body

  https://pokeapi.co/api/v2/pokemon/charizard

let charizard2 =
  let uri = Uri.of_string ("https://pokeapi.co/api/v2/pokemon/charizard") in
  Client.call `GET uri >>= fun (_, body) ->
  body |> Cohttp_async.Body.to_string >>| fun body -> body

(******)

(* API calls *)

let get_reqBody_api1 name =
  let uri = Uri.of_string ("https://pokeapi.co/api/v2/pokemon-species/"^name) in
  Client.call `GET uri >>= fun (_, body) ->
  body |> Cohttp_async.Body.to_string >>| fun body -> body

let get_reqBody_api2 name =
  let uri = Uri.of_string ("https://pokeapi.co/api/v2/pokemon/"^name) in
  Client.call `GET uri >>= fun (_, body) ->
  body |> Cohttp_async.Body.to_string >>| fun body -> body

  (* PARSING JSON: generally useful functions*)

(* Extract a given field from the query result *)
(* note: we can also do a function that takes a list of fields and returns a list of Option string *)
(* note this wraps any type in a string!! *)
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

  let find_field_string field json = 
    match json with
    | `Assoc kv_list -> 
          (begin match List.Assoc.find ~equal:String.equal kv_list field with
          | Some (`String s) -> Some s
          | _ -> None
          end)     
    | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ -> None

  let find_field_json field json = 
    match json with
    | `Assoc kv_list -> List.Assoc.find ~equal:String.equal kv_list field     
    | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ -> None
  
  let find_name json = find_field_string "name" json  

  let get_field_list field json_str =
    match Yojson.Safe.from_string json_str with
    | `Assoc kv_list -> 
          match List.Assoc.find ~equal:String.equal kv_list field with
          | Some (`List json_ls) -> json_ls
          | _ -> []
    | _ ->  []
  
    (* takes a single json and determines if its language is English*)
  let is_english json = 
    begin match json with
    | `Assoc kv_list -> 
          begin match List.Assoc.find ~equal:String.equal kv_list "language" with
          | Some js -> 
            begin match find_name js with
            | Some "en" -> true
            | _ -> false
            end   
          | _ -> false
          end
    | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ ->  false
    end

  (* PARSING JSON: entry-specific functions *)

  (* API 1 *)

(* is_legendary *)

let get_is_legendary json_str = 
  json_str >>| fun str -> get_field_from_json "is_legendary" str

(* habitat *)
let get_habitat_name json_str = 
  (json_str >>| fun str -> get_field_from_json "habitat" str) 
  >>| (fun opt_str -> Option.bind opt_str (fun str -> get_field_from_json "name" str))

(* description (flavor_text) *)

(*let get_flavor_text_list json_str =
  match Yojson.Safe.from_string json_str with
  | `Assoc kv_list -> 
        match List.Assoc.find ~equal:String.equal kv_list "flavor_text_entries" with
        | Some (`List json_ls) -> json_ls
        | _ -> []
  | _ ->  []*)

let pick_first_description ls = 
  begin match ls with
  | json :: jsons -> 
    begin match find_field_string "flavor_text" json with 
    | Some descr -> descr
    | None -> "NA"
    end
  | [] -> "NA"
  end

let get_flavor_text json_str =
  (json_str >>| fun str -> get_field_list "flavor_text_entries" str) 
  >>| (fun ls -> List.filter ls (fun json -> is_english json))
  >>| (fun ls -> pick_first_description ls)
 (* still need to parse this string*)

(* API 2 *)

(* height *)
let get_height json_str = 
  json_str >>| fun str -> get_field_from_json "height" str

(* types *)
let get_type json = 
  match Option.bind (find_field_json "type" json) (fun js -> find_name js) with
  | Some s -> s
  | None -> ""


let get_types json_str =
  (json_str >>| fun str -> get_field_list "types" str) >>| (* works until here*)
  fun ls -> List.map ls (fun js -> get_type js) 
  (* >>| 
  (fun ls -> List.filter ls (fun str -> not (String.equal str "")))*)


(*let get_color = 
  (reqBody >>| fun str -> get_field_from_json "color" str)

let get_color_name = 
  (get_color >>| (fun opt_str -> Option.bind opt_str (fun str -> get_field_from_json "name" str)))

let get_color_name' name = 
  ((get_reqBody_api1 name) >>| fun str -> get_field_from_json "color" str) 
  >>| (fun opt_str -> Option.bind opt_str (fun str -> get_field_from_json "name" str)) *)

(* let print_color_name = 
  (get_color >>| (fun opt_str -> Option.bind opt_str (fun str -> get_field_from_json "name" str)))
  >>| (fun opt_str -> Option.iter opt_str (fun str -> print_endline str))

let print_color_name' = 
  (get_color >>| (fun opt_str -> Option.bind opt_str (fun str -> get_field_from_json "name" str)))
  >>| (fun opt_str -> Option.map opt_str (fun str -> str)) *)



let () = 
  let _ =  print_color_name in
  (*don't_wait_for (exit 0);*)
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


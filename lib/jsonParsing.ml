open Core
open Async 
open Yojson
open Cohttp_async

(* PARSING JSON: generally useful functions*)

  (* find functions take a json file of type json as argument *)

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

  (* get functions take a string representing json file as argument*)

  let get_field_string field json_str = 
    match Yojson.Safe.from_string json_str with
    | `Assoc kv_list -> 
          (begin match List.Assoc.find ~equal:String.equal kv_list field with
          | Some (`String s) -> Some s
          | _ -> None
          end)     
    | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ -> None

  let get_field_json field json_str = 
    match Yojson.Safe.from_string json_str with
    | `Assoc kv_list -> List.Assoc.find ~equal:String.equal kv_list field     
    | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ -> None
  
  let get_field_int field json_str = 
    begin match Yojson.Safe.from_string json_str with
    | `Assoc kv_list -> 
          (begin match List.Assoc.find ~equal:String.equal kv_list field with
          | Some (`Int i) -> Some i
          | _ -> None
          end)     
    | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ -> None
    end

  let get_field_bool field json_str = 
    begin match Yojson.Safe.from_string json_str with
    | `Assoc kv_list -> 
          (begin match List.Assoc.find ~equal:String.equal kv_list field with
          | Some (`Bool b) -> Some b
          | _ -> None
          end)     
    | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ -> None
    end

  let get_field_list field json_str =
    begin match Yojson.Safe.from_string json_str with
    | `Assoc kv_list -> 
          begin match List.Assoc.find ~equal:String.equal kv_list field with
          | Some (`List json_ls) -> json_ls
          | _ -> []
          end
    | `Tuple _ | `Bool _ | `Intlit _ | `Null| `Variant (_, _)| `List _ |`Float _ | `String _ | `Int _ ->  []
    end
  
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
  json_str >>| fun str -> get_field_bool "is_legendary" str

(* habitat *)

let get_habitat json_str = 
  (json_str >>| fun str -> get_field_json "habitat" str) 
  >>| (fun opt_str -> Option.bind opt_str (fun str -> find_field_string "name" str))


(* description (flavor_text) *)

let pick_first_description ls = 
  begin match ls with
  | json :: jsons -> find_field_string "flavor_text" json
  | [] -> None
  end

let get_flavor_text json_str =
  (json_str >>| fun str -> get_field_list "flavor_text_entries" str) 
  >>| (fun ls -> List.filter ls (fun json -> is_english json))
  >>| (fun ls -> pick_first_description ls)

(* API 2 *)

(* height *)

let get_height json_str = 
  json_str >>| fun str -> get_field_int "height" str

(* types *)

let get_type json = 
  match Option.bind (find_field_json "type" json) (fun js -> find_name js) with
  | Some s -> s
  | None -> ""


let get_types json_str =
  ((json_str >>| fun str -> get_field_list "types" str) >>| 
  fun ls -> List.map ls (fun js -> get_type js)) >>| 
  (fun ls -> match ls with 
              | hd :: tl -> Some (hd :: tl)
              | [] -> None)



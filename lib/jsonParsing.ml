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

(* name *)

let get_name json_str_opt = 
  Option.bind json_str_opt (fun s -> get_field_string "name" s)

(* is_legendary *)

let get_is_legendary json_str_opt = 
  Option.bind json_str_opt (fun s -> get_field_bool "is_legendary" s)

(* habitat *)

let get_habitat json_str_opt = 
  let json_opt = Option.bind json_str_opt (fun s -> get_field_json "habitat" s) (* Safe.t option *)
  in Option.bind json_opt (fun str -> find_field_string "name" str)


(* description (flavor_text) *)

let pick_first_description ls = 
  begin match ls with
  | json :: jsons -> find_field_string "flavor_text" json
  | [] -> None
  end

let get_flavor_text json_str_opt =
  let list_opt = Option.map json_str_opt (fun str -> get_field_list "flavor_text_entries" str) in (* Safe.t list option *)
  let eng_list_opt = Option.map list_opt (fun ls -> List.filter ls (fun json -> is_english json)) in 
  Option.bind eng_list_opt (fun ls -> pick_first_description ls)


(* API 2 *)

(* height *)

let get_height json_str_opt = 
  Option.bind json_str_opt (fun str -> get_field_int "height" str)

(* types *)

let get_type json = 
  match Option.bind (find_field_json "type" json) (fun js -> find_name js) with
  | Some s -> s
  | None -> ""

let get_types json_str_opt =
  let opt_json_list = Option.map json_str_opt (fun str -> get_field_list "types" str) in 
  let op_str_list = Option.map opt_json_list (fun ls -> List.map ls (fun js -> get_type js)) in 
  Option.bind op_str_list (fun ls -> match ls with 
                                      | hd :: tl -> Some (hd :: tl)
                                      | [] -> None)



(* PRINTERS *)

(* all have type 'a * string option -> 'a * string option = <fun> with side effect of printing *)

(* TODO: generalise this into more general functions and printer type*)

let name_printer name_json_pair = 
  let (name, opt_json) = name_json_pair in
  let opt_name = get_name opt_json in
  begin match opt_name with (* in case the json is a valid json it is still worth checking if it has info about this pokemon *)
  | None -> print_endline "Invalid pokemon name! \n"; (name, None)
  | Some s -> printf "name: %s \n" s; (name, opt_json)
  end


let is_legendary_printer name_json_pair = 
  let (name, opt_json) = name_json_pair in
  begin match opt_json with
  | None -> (name, opt_json)
  | Some json -> 
    begin match get_is_legendary opt_json with
    | None -> print_endline "is_legendary: NA \n"
    | Some b -> printf "is_legendary: %b \n" b
    end; 
    (name, opt_json)
  end

let habitat_printer name_json_pair = 
  let (name, opt_json) = name_json_pair in
  begin match opt_json with
  | None -> (name, opt_json)
  | Some json -> 
    begin match get_habitat opt_json with
    | None -> print_endline "habitat: NA \n"
    | Some s -> printf "habitat: %s \n" s
    end; 
    (name, opt_json)
  end

let description_printer name_json_pair = 
  let (name, opt_json) = name_json_pair in
  begin match opt_json with
  | None -> (name, opt_json)
  | Some json -> 
    begin match get_flavor_text opt_json with
    | None -> print_endline "description: NA \n"
    | Some s -> printf "description: %s \n" s
    end; 
    (name, opt_json)
  end

let height_printer name_json_pair = 
  let (name, opt_json) = name_json_pair in
  let opt_height = get_height opt_json in
  begin match opt_height with (* in case the json is a valid json it is still worth checking if it has info about this pokemon *)
  | None -> (name, None)
  | Some x -> printf "height: %i \n" x; (name, opt_json)
  end

let unparse_list ls = (* for a list of strings only!! *)
  begin match ls with
  | [] -> "[]"
  | hd :: tl -> 
    let rec unparse vs acc =
      begin match vs with
      | [] -> acc^"]"
      | v :: vs' -> unparse vs' (acc^v^",")
      end
    in unparse ls "["
  end

let types_printer name_json_pair = 
  let (name, opt_json) = name_json_pair in
  begin match opt_json with
  | None -> (name, opt_json)
  | Some json -> 
    begin match get_types opt_json with
    | None -> print_endline "types: NA \n"
    | Some ls -> printf "types: %s \n" (unparse_list ls)
    end; 
    (name, opt_json)
  end









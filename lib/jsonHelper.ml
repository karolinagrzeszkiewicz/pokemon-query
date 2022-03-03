 open Yojson
 open Core

(* PARSING JSON: generally useful functions*)

(* find functions take a json file of type json as argument *)

let find_field_string (field : string) (json : Safe.t) : string option = 
  match json with
  | `Assoc kv_list -> 
        (begin match List.Assoc.find ~equal:String.equal kv_list field with
        | Some (`String s) -> Some s
        | _ -> None
        end)     
  | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ -> None

let find_field_json (field : string) (json : Safe.t) : Safe.t option = 
  match json with
  | `Assoc kv_list -> List.Assoc.find ~equal:String.equal kv_list field     
  | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ -> None


let find_name (json : Safe.t) : string option = find_field_string "name" json 

(* get functions take a string representing json file as argument*)

let get_field_string (field : string) (json_str : string) : string option = 
  match Yojson.Safe.from_string json_str with
  | `Assoc kv_list -> 
        (begin match List.Assoc.find ~equal:String.equal kv_list field with
        | Some (`String s) -> Some s
        | _ -> None
        end)     
  | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ -> None

let get_field_json (field : string) (json_str : string) : Safe.t option = 
  match Yojson.Safe.from_string json_str with
  | `Assoc kv_list -> List.Assoc.find ~equal:String.equal kv_list field     
  | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ -> None

let get_field_int (field : string) (json_str : string) : int option = 
  begin match Yojson.Safe.from_string json_str with
  | `Assoc kv_list -> 
        (begin match List.Assoc.find ~equal:String.equal kv_list field with
        | Some (`Int i) -> Some i
        | _ -> None
        end)     
  | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ -> None
  end

let get_field_bool (field : string) (json_str : string) : bool option = 
  begin match Yojson.Safe.from_string json_str with
  | `Assoc kv_list -> 
        (begin match List.Assoc.find ~equal:String.equal kv_list field with
        | Some (`Bool b) -> Some b
        | _ -> None
        end)     
  | `Bool _ | `String _ | `List _ | `Float _ | `Int _ | `Null | _ -> None
  end

let get_field_list (field : string) (json_str : string) : Safe.t list =
  begin match Yojson.Safe.from_string json_str with
  | `Assoc kv_list -> 
        begin match List.Assoc.find ~equal:String.equal kv_list field with
        | Some (`List json_ls) -> json_ls
        | _ -> []
        end
  | `Tuple _ | `Bool _ | `Intlit _ | `Null| `Variant (_, _)| `List _ |`Float _ | `String _ | `Int _ ->  []
  end

(* takes a single json and determines if its language is English*)
let is_english (json : Safe.t) : bool = 
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

(* helper function for unparsing lists *)
let unparse_list (ls : string list) : string = 
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
 open Yojson
 open Core

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

  (* helper function for unparsing lists *)
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
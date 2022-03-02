open Yojson
open Core
open JsonHelper

(* Parser module *)

module type Parser = sig
  type output
  val label : string
  val parser : string option -> output option
  val print_not_found : string -> unit 
  val print_found : string -> output -> unit 
end

(* PARSING JSON: entry-specific *)

(* API 1 *)

(* name *)

module NameParser : Parser = struct

  type output = string

  let label = "name"

  let parser json_str_opt = Option.bind json_str_opt (fun s -> get_field_string "name" s)

  let print_not_found label = ()

  let print_found label value = printf "%s: %s \n \n" label value

end

(* is_legendary *)

module IsLegendaryParser : Parser = struct

  type output = bool

  let label = "is_legendary"

  let parser json_str_opt = Option.bind json_str_opt (fun s -> get_field_bool "is_legendary" s)

  let print_not_found label = printf "%s: NA \n \n" label

  let print_found label value = printf "%s: %b \n \n" label value

end


(* habitat *)

module HabitatParser : Parser = struct

  type output = string

  let label = "habitat"

  let parser json_str_opt = 
    let json_opt = Option.bind json_str_opt (fun s -> get_field_json "habitat" s) 
    in Option.bind json_opt (fun str -> find_field_string "name" str)

  let print_not_found label = printf "%s: NA \n \n" label

  let print_found label value = printf "%s: %s \n \n" label value

end


(* description (flavor_text) *)

module DescriptionParser : Parser = struct

  type output = string

  let label = "description"

  let parser json_str_opt = 
    let pick_first_description ls = 
      begin match ls with
      | json :: jsons -> find_field_string "flavor_text" json
      | [] -> None
      end 
    in let list_opt = Option.map json_str_opt (fun str -> get_field_list "flavor_text_entries" str) in 
    let eng_list_opt = Option.map list_opt (fun ls -> List.filter ls (fun json -> is_english json)) in 
    Option.bind eng_list_opt (fun ls -> pick_first_description ls)
    

  let print_not_found label = printf "%s: NA \n \n" label

  let print_found label value = printf "%s: %s \n \n" label value

end


(* API 2 *)

(* height *)

module HeightParser : Parser = struct

  type output = int

  let label = "height"

  let parser json_str_opt = Option.bind json_str_opt (fun str -> get_field_int "height" str)

  let print_not_found label = ()

  let print_found label value = printf "%s: %i \n \n" label value

end

(* types *)

module TypesParser : Parser = struct

  type output = string list

  let label = "types"

  let parser json_str_opt = 
    let get_type json = 
      match Option.bind (find_field_json "type" json) (fun js -> find_name js) with
      | Some s -> s
      | None -> ""
    in let opt_json_list = Option.map json_str_opt (fun str -> get_field_list "types" str) in 
    let op_str_list = Option.map opt_json_list (fun ls -> List.map ls (fun js -> get_type js)) in 
    Option.bind op_str_list (fun ls -> match ls with 
                                        | hd :: tl -> Some (hd :: tl)
                                        | [] -> None)

  let print_not_found label = printf "%s: NA \n \n" label

  let print_found label value = printf "%s: %s \n \n" label (unparse_list value)

end











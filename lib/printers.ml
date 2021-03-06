open Yojson
open Core
open JsonHelper
open JsonParsing


(* PRINTERS *)

module Printer(Parse: Parser) = struct

  include Parse

  (* if the input json is None we don't print anything, 
  if the json is Some then we either print the entry if it is found or not found if it is not found*)
  let print (name_json_pair : string * string option) : string * string option = 
    let (name, opt_json) = name_json_pair in
    begin match opt_json with
    | None -> (name, opt_json)
    | Some json -> 
      begin match parser opt_json with
      | None -> print_not_found label; (name, None)
      | Some value -> print_found label value; (name, opt_json)
      end; 
    end


end

module NamePrinter = Printer(NameParser)

module IsLegendaryPrinter = Printer(IsLegendaryParser)

module HabitatPrinter = Printer(HabitatParser)

module DescriptionPrinter = Printer(DescriptionParser)

module HeightPrinter = Printer(HeightParser)

module TypesPrinter = Printer(TypesParser)

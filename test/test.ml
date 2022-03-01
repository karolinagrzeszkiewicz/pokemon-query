
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

let charizard2 =
  let uri = Uri.of_string ("https://pokeapi.co/api/v2/pokemon/charizard") in
  Client.call `GET uri >>= fun (_, body) ->
  body |> Cohttp_async.Body.to_string >>| fun body -> body

(******)
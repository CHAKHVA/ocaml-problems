let rec last = function [] -> None | [ x ] -> Some x | _ :: t -> last t

let rec last_two = function
  | [] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t

let rec nth list n =
  match (list, n) with
  | h :: _, 0 -> Some h
  | [], _ -> raise (Failure "nth")
  | _ :: t, x -> nth t (x - 1)

let rec length = function [] -> 0 | _ :: t -> 1 + length t

let rev list =
  let rec aux acc = function [] -> acc | h :: t -> aux (h :: acc) t in
  aux [] list

let is_palindrome list = list = rev list

type 'a node = One of 'a | Many of 'a node list

let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  rev (aux [] list)

let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller

let pack list =
  let rec aux current acc = function
    | [] -> acc
    | [ x ] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (a :: current) acc t
        else aux [] ((a :: current) :: acc) t
  in
  aux [] [] list

let encode list =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in
  rev (aux 0 [] list)

let () =
  let _ = last [ "a"; "b"; "c"; "d" ] in
  let _ = last [] in
  let _ = last_two [ "a"; "b"; "c"; "d" ] in
  let _ = last_two [ "a" ] in
  let _ = nth [ "a"; "b"; "c"; "d"; "e" ] 2 in
  let _ = length [ "a"; "b"; "c" ] in
  let _ = length [] in
  let _ = rev [ "a"; "b"; "c" ] in
  let _ = is_palindrome [ "a"; "b"; "c" ] in
  let _ = not (is_palindrome [ "a"; "b" ]) in
  let _ =
    flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  in
  let _ =
    compress
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  let _ =
    pack
      [
        "a";
        "a";
        "a";
        "a";
        "b";
        "c";
        "c";
        "a";
        "a";
        "d";
        "d";
        "e";
        "e";
        "e";
        "e";
      ]
  in
  let _ =
    encode
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  ()

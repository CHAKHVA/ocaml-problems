let rec last = function [] -> None | [ x ] -> Some x | _ :: t -> last t
let option_to_value = function None -> "" | Some x -> x

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

let () =
  print_string (option_to_value (last [ "a"; "b"; "c"; "d" ]));
  let _ = last [] in
  let _ = last_two [ "a"; "b"; "c"; "d" ] in
  let _ = last_two [ "a" ] in
  let _ = nth [ "a"; "b"; "c"; "d"; "e" ] 2 in
  let _ = length [ "a"; "b"; "c" ] in
  let _ = length [] in
  let _ = rev [ "a"; "b"; "c" ] in
  ()

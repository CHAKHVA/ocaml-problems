let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t

let () = let _ = last ["a" ; "b" ; "c" ; "d"] in ()

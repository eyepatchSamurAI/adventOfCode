open Core

(* let rec drop n theList =
  match theList with
  | [] -> []
  | _ :: tail when n > 0 -> drop (n-1) tail
  | _ -> theList *)

let rec drop n lst =
  match lst with
  | [] -> [] (* If the list is empty, return an empty list *)
  | _ :: tl -> if n > 0 then drop (n-1) tl else lst


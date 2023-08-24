open Printf

let () =
  (* Define the range function *)
  let rec range a b =
    if a > b then []
    else a :: range (a + 1) b
  in

  let rec factorial n = 
    match n with
    | 0 | 1 -> 1
    | x -> x * factorial (x - 1)
  in

  let rec fib n = 
    match n with
    | 0 -> 0
    | 1 -> 1
    | x -> fib (x - 2) + fib (x -1)
  in

  let rec total l =
    match l with
    | [] -> 0
    | h :: t -> h + total t
  in
  
  (* Use the range function to define digits *)
  let digits = range 0 9 in
  let fac_3 = factorial 3 in
  
  (* Use digits in some way, e.g., print them *)
  List.iter (printf "%d ") digits;
  print_newline ();
  print_endline "_";
  print_int (fib 6);
  print_newline ();
  print_int fac_3;
  print_newline ();
  print_endline "_";

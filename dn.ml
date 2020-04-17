(* 
debt network after 
A Solution to Post Crash Debt Entanglements in Kuwait's al-Manakh Stock Market
A. A. Elimam, M. Girgis, S. Kotob
1997
*)

open Array;;

(* auxiliary functions *)
let sum_l  = 
  Array.fold_left
    (fun x e -> x + e)
    0
;;

let pretty_print_int j e =
  if j = 0
  then print_string "| ";
  print_int e;
  print_char ' ';
  if j = 3 then print_string "|\n"
;;

let pretty_print_float j e =
(*  if j = 0
    then print_string "| ";*)
  print_float e;
  print_char ' ';
  if j = 3 then print_string "\n"
;;

(* debt network example *)
let dn = Array.make_matrix 4 4 0;;
let df = Array.make_matrix 4 4 0.;;
let asset = Array.make 4 0;;

let reset_asset () =
  asset.(0) <- 10;
  asset.(1) <- 3;
  asset.(2) <- 85;
  asset.(3) <- 30;;

let reset_matrix () =
  dn.(0) <- [|0;9;74;0|];
  dn.(1) <- [|42;0;23;0|];
  dn.(2) <- [|5;40;0;0|];
  dn.(3) <- [|15;15;20;0|];;

let reset_df () =
for i = 0 to 3
do
  for j = 0 to 3
  do let weight = sum_l dn.(i) in
     if (weight = 0)
     then df.(i).(j) <- 0.
     else df.(i).(j) <-
            (float_of_int  dn.(i).(j))
            /. (float_of_int weight)
  done
done  
;;

(* printing *)
let pdn () =
  Array.iter
    (fun ia -> Array.iteri pretty_print_int ia)
    dn
;;

let pa () =
  Array.iteri pretty_print_int asset
;;

let pad () =
  print_string "asset = \n"; pa();
  print_string "debt  = \n"; pdn()
;;

(* set and reset *)
let reset () =
  reset_asset  ();
  reset_matrix ();
  reset_df ();
  pad()
;;

reset ()
;;

(* transfers *)
let rp i j a =
  if (asset.(i) < a)
  then failwith "not enough money";
  if (dn.(i).(j) < a)
  then failwith "too much money";
  asset.(i) <- asset.(i) - a;
  asset.(j) <- asset.(j) + a;
  dn.(i).(j) <- dn.(i).(j) - a;; 
  

(* 1. D pays *)
rp 3 1 15;;
rp 3 0 15;;
try
  rp 3 2 20
with _ -> ()
;; (* Exception: Failure "not enough money". *)

(* 2. C pays *)
rp 2 0 5;;
rp 2 1 40;;

(* B is not solvent yet *)

(* 3. A pays B *)
rp 0 1 9;;

(* B pays *)
rp 1 0 42;;
rp 1 2 23;;

(* A under-pays C *)
rp 0 2 63;;

(*
asset = |0 2 126 0|

total fusion value is invariant

residual debt  = 
        |0 0 11  0|
        |0 0  0  0|
        |0 0  0  0|
        |0 0 20  0|
*)


(* clearing vector map *)

(* ideal dues - static *)
(* should compute it from dn *)
let dues = 
Array.of_list
     (List.map float_of_int [83;65;45;50])
;;

(* returns the amount of money collected by node i under clearing vector du *)
let phi i du = 
  let res = ref 0.
  in
    for j = 0 to 3
    do
      res := !res +. du.(j) *. (df.(j).(i))
    done;
    res := !res +. (float_of_int asset.(i));
    print_float (!res);
    print_string "; ";
    if (!res >= dues.(i))
    then res := dues.(i);
    !res
;;

(* returns the actual amount paid *)
let psi du =
  print_string "\n";
  let a1 = Array.init 4 (fun i -> phi i du)
  in
  Array.mapi
    (fun i x ->
      if (x <= dues.(i))
      then x       (* limited liability *)
      else dues.(i)
    )
    a1
;;


reset ();;

let rec iterate n a =
  match n
  with 0 -> print_char '\n'; (Array.iteri pretty_print_float a)
     | k  -> iterate (k-1) (psi a)
;;


(*
due        [|83.; 65. ;   45.; 50.|]
> stage 1 - A, D become insolvent  
collected  [|72.; 67. ;  202.; 30.|]
paid       [|72.; 65. ;   45.; 30.|]
> stage 2 - B becomes insolvent too
collected  [|66.; 59.8; 184.2; 30.|]
paid       [|66.; 59.8;  45.0; 30.|]
> stage  3- 
collected  [|62.6; 59.2; 177.0; 30.|]
paid       [|62.6; 59.2;  45.0; 30.|]
*)

(* 
iteration 10
[|61.9399; 58.71639; 45.; 30.|]
iteration 20
[|61.9398; 58.71636; 45.; 30.|]

Note the decreasing  monotonicity
(we are computing a greatest fixed point)

all the value has gone to C!
down from the expected 202 - 45 = 157
to 128

should replay the experiment with an alpha/beta
maybe a phase transition?

*)

(*       works also with streams
 *       CDSs make everything more difficult
 *       fp algo converges fast modulo solving the linear system
 *       model cycle: place bets - run them to term - solve for fp
 *       this is code - so we can make it an sc!
*)

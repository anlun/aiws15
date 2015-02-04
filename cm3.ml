type var = X | Y | Z

type inst = Inc of var
          | Dec of var
          | Zero of var * int * int
          | Stop

module Parser =
struct
  open ParserComb
  let varP  = let symP c r = map (fun _ -> r) (char c) in
              sp (symP 'x' X <|> symP 'y' Y <|> symP 'z' Z)
  let incP  = symbol ~&"inc"  >> map (fun x -> Inc x) varP
  let decP  = symbol ~&"dec"  >> map (fun x -> Dec x) varP
  
  let zeroP = sp (symbol ~&"zero") >> varP >>= fun x ->
              sp (number) >>= fun m ->
              symbol ~&"else" >>
              sp(number) >>= fun n ->
              mreturn (Zero (x, m, n))

  let stopP = symbol ~&"stop" >> mreturn Stop

  let instP = incP <|> decP <|> zeroP <|> stopP
  let progP (s : string) = many1 instP ~&s 
end

module PrettyPrinter =
struct
  let varPP  v = match v with X -> "x" | Y -> "y" | Z -> "z"
  let instPP i =
    match i with
    | Inc x -> "inc " ^ (varPP x)
    | Dec x -> "dec " ^ (varPP x)
    | Stop  -> "stop"
    | Zero (x, m, n) -> "zero " ^ (varPP x) ^ " " ^
                        (string_of_int m) ^ " else " ^ (string_of_int n)

  let progPP p =
    "Program:\n\n" ^
    let counter = ref (1 + (List.length p)) in
    List.fold_right (
      fun v acc ->
      counter := !counter - 1 ;
      (Printf.sprintf "%3i: " !counter) ^ (instPP v) ^ "\n" ^ acc
    ) p ""
end

type pc = int (* program counter *)
type state = pc * int * int * int

type program = inst list

module Interpreter =
struct
  let step (s : state) (p : program) =
    (* current instruction *)
    let (m, x, y, z) = s in
    let i = List.nth p (m - 1) in
    match i with
    | Inc X -> (m+1, x+1, y, z)
    | Dec X -> (m+1, x-1, y, z)
    
    | Inc Y -> (m+1, x, y+1, z)
    | Dec Y -> (m+1, x, y-1, z)
    
    | Inc Z -> (m+1, x, y, z+1)
    | Dec Z -> (m+1, x, y, z-1)

    | Zero (X, m, n) -> if x == 0
                        then (m, x, y, z)
                        else (n, x, y, z)

    | Zero (Y, m, n) -> if y == 0
                        then (m, x, y, z)
                        else (n, x, y, z)
    
    | Zero (Z, m, n) -> if z == 0
                        then (m, x, y, z)
                        else (n, x, y, z)
                               
    | Stop -> (0, x, y, z)
  
  let statePP s =
    let (m, x, y, z) = s in
    Printf.sprintf "  (%3i,%3i,%3i,%3i)" m x y z 
  
  let interpret (i : int) (p : program) =
    Printf.printf "Interpreter steps:\n\n";
    let state = ref (1, i, 0, 0) in
    let m = ref 1 in
    while !m <> 0 do
      Printf.printf "%s\n" (statePP !state);
      state := step !state p;
      let (nm, _, _, _) = !state in
      m := nm
    done;
    Printf.printf "%s\n" (statePP !state)
end

let pr_ex1 =
  "inc x
   dec x
   stop"

let pr_ex2 =
  "zero x 6 else 2 
   dec x
   inc y
   inc y
   zero x 6 else 2
   stop"

open PrettyPrinter
open Parser
open Interpreter
(*       
let () =
  (*let pr_text = Printf.*)
  let pr1 = fst (List.hd (progP pr_ex2)) in 
  Printf.printf "%s\n" (progPP pr1);
  interpret 1 pr1 
 *)

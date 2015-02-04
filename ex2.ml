type var = X | Y | Z

type inst = Inc of var
          | Dec of var
          | Zero of var * int * int
          | Stop

module CoreParser =
struct
  type 'a t = char list -> ('a * char list) list

  let (>>=) (p: 'a t) (f: 'a -> 'b t) = fun s ->
    List.concat (List.map (fun (r, s') -> (f r) s')(p s))

  let mreturn r = fun s -> [(r, s)]
  let lambda    = fun s -> []
  let item      = fun s -> match s with [] -> [] | h :: s -> [(h, s)]
  let sat cond  = item >>= fun c -> if cond c then mreturn c else lambda
  
  let (>>) p q = p >>= fun _ -> q
  let (<<) p q = p >>= fun rs -> q >> mreturn rs
                                              
  let char c    = sat ((=) c)
  let digit     = sat (fun c ->
                        match c with
                        | '0' .. '9' -> true
                        | _ -> false
                      )
  let alpha     = sat (fun c ->
                        match c with
                        | 'a' .. 'z' | 'A' .. 'Z' -> true
                        | _ -> false
                      )
  let (<|>) p q = fun s ->
    match p s with
    | [] -> q s
    | rs -> rs  
  let (++) p q = fun s -> List.append (p s) (q s)
  
  let rec many0 p = many1 p <|> mreturn []
  and many1 p = p >>= fun r -> many0 p >>= fun rs -> mreturn (r::rs)

  let rec symbol cs =
    match cs with
    | [] -> mreturn [] 
    | c::cs' -> char c >> symbol cs' >> mreturn cs

  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [];;

  let implode l =
    let res = String.create (List.length l) in
    let rec imp i = function
    | [] -> res
    | c :: l -> res.[i] <- c; imp (i + 1) l in
    imp 0 l;;

  let (~&) (s: string   ) = explode s
  let (~%) (l: char list) = implode l
  
  let map (f: 'a -> 'b) (p: 'a t): 'b t = fun s ->
    p s |> List.map (fun (e, s) -> (f e, s)) 
  
  let number = map (fun s -> int_of_string ~%s) (many1 digit) 
  let word   = map (~%) (many1 alpha)
  let spaces = many0 (char ' ' <|> char '\t' <|> char '\n' <|> char '\010')
  let sp   f = spaces >> f << spaces 
  
  let paren p =
    let lparen = char '(' in
    let rparen = char ')' in
    sp lparen >>= (fun _ -> p() << sp rparen)
  let paren' p = paren (fun _ -> p)  
  
  let cparen p =
    let lcparen = char '{' in
    let rcparen = char '}' in
    sp lcparen >>= (fun _ -> p() << sp rcparen)
  let cparen' p = cparen (fun _ -> p)  
end

module Parser =
struct
  open CoreParser
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

module Interpreter =
struct
  type pc = int (* program counter *)
  type state = pc * int * int * int

  type program = inst list

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
       
let () =
  (*let pr_text = Printf.*)
  let pr1 = fst (List.hd (progP pr_ex2)) in 
  Printf.printf "%s\n" (progPP pr1);
  interpret 1 pr1 

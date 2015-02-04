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
  let res = Bytes.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> Bytes.set res i c; imp (i + 1) l in
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

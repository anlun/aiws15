open Matcher
open String
open Str
open Ostap 

(* Grammar expression *)
let p = rule <name>=IDENT -"(" (<h>=IDENT <t>=(-"," IDENT)* {h::t}) -")" end

(* Whitespace and identifier regular patterns *)
let ws      = regexp "[ \n\t\r]+"
let ident   = regexp "[a-zA-Z_]\([a-zA-Z_0-9]\)*"

(* Lexer definition *)
class lexer s p coord =
    object (self)
       (* Magic phrase to inherit from predefined matcher *)
       inherit [lexer] matcher (fun s p coord -> new lexer s p coord) s p coord

       (* Skipping whitespaces *)
       method skip =          
          if string_match ws s p
	  then
	    let m = matched_string s in
	    (p+length m), (shiftPos coord m 0 (length m))
	  else p, coord

       (* Matching identifiers *)
       method getIDENT = self#get "identifier" ident

    end

(* Stream constructor *)
let ofString s = new lexer s 0 (1, 1)

(* Parsing driver *)
let parse s =
  let module P = View.NamedPair (struct let first = "name" let second = "args" end) (Token) (View.List (Token)) in
  match p (ofString s) with
  | Parsed (x, _) -> Printf.printf "Success: %s\n" (P.toString x)
  | Failed msgs | Error msgs -> Printf.printf "Unsuccess: %s\n" (let module M = View.List (Msg) in M.toString msgs)

(* Entry point *)
let _ =
  parse "a (b, c, d)";
  parse "a"
(* Lexical analyser *)

{
  open Lexing
  open Parser

  let int_of_string x =
    try int_of_string x with _ ->
      Printf.printf "Cannot convert to int: %s\n" x;
      0

}

let digit = ['0'-'9']
let integer = digit+
let mul = "mul"
let do = "do()"
let dont = "don't()"

rule token = parse
  | '\n'                  { token lexbuf }
  | mul '(' (integer as a)
        ',' (integer as b)
        ')'               { MUL (int_of_string a, int_of_string b) }
  | do                    { DO }
  | dont                  { DONT }
  | _                     { token lexbuf }
  | eof                   { EOF }

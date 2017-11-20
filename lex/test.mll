{
    (* short names for important modules *)
    module L = Lexing 
    module B = Buffer

    type token = 
    | STR of string
    | INT of int
    | ID of string
    | PLUSEQ
    | MINUSEQ
    | STAREQ
    | SLASHEQ
    | PLUS
    | MINUS
    | STAR
    | SLASH
    | ASSIGN
    | EOF (* end of input *)


    let get      = L.lexeme
let sprintf  = Printf.sprintf

let position lexbuf =
    let p = lexbuf.L.lex_curr_p in
        sprintf "%s:%d:%d" 
        p.L.pos_fname p.L.pos_lnum (p.L.pos_cnum - p.L.pos_bol)

let set_filename (fname:string) (lexbuf:L.lexbuf)  =
    ( lexbuf.L.lex_curr_p <-  
        { lexbuf.L.lex_curr_p with L.pos_fname = fname }
    ; lexbuf
    )

exception Error of string
let error lexbuf fmt = 
    Printf.kprintf (fun msg -> 
        raise (Error ((position lexbuf)^" "^msg))) fmt

}
let ws    = [' ' '\t']
let nl    = ['\n']

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id    = alpha (alpha|digit)*

rule token = parse
| ws+       { token lexbuf  }
| nl        { L.new_line lexbuf; token lexbuf }
| digit+    { INT(int_of_string @@ get lexbuf) }
| id        { ID(get lexbuf)}
| '+'       { PLUS          }
| '-'       { MINUS         }
| '*'       { STAR          }
| '/'       { SLASH         }
| "+="      { PLUSEQ        }
| "-="      { MINUSEQ       }
| "*="      { STAREQ        }
| "/="      { SLASHEQ       }
| ":="      { ASSIGN        }
| '"'       { STR (string (B.create 100) lexbuf) } (* see below *)

| eof       { EOF           }
| _         { error lexbuf 
               "found '%s' - don't know how to handle" @@ get lexbuf }
 
and escape b = parse
  | '&'             { Buffer.add_string b "&amp;";  escape b lexbuf } 
  | '"'             { Buffer.add_string b "&quot;"; escape b lexbuf } 
  | '\''            { Buffer.add_string b "&apos;"; escape b lexbuf }
  | '>'             { Buffer.add_string b "&gt;";   escape b lexbuf }
  | '<'             { Buffer.add_string b "&lt;";   escape b lexbuf }
  | [^'&' '"' '\'' '>' '<']+ 
                    { Buffer.add_string b @@ get lexbuf
                    ; escape b lexbuf
                    }
  | eof             { let x = Buffer.contents b in Buffer.clear b; x }
  | _               { error lexbuf 
                        "don't know how to quote: %s" (get lexbuf) }

and string buf = parse (* use buf to build up result *)
| [^'"' '\n' '\\']+  
            { B.add_string buf @@ get lexbuf
            ; string buf lexbuf 
            }
| '\n'      { B.add_string buf @@ get lexbuf
            ; L.new_line lexbuf
            ; string buf lexbuf
            }
| '\\' '"'  { B.add_char buf '"'
            ; string buf lexbuf
            }
| '\\'      { B.add_char buf '\\'
            ; string buf lexbuf
            }
| '"'       { B.contents buf } (* return *)
| eof       { error lexbuf "end of input inside of a string" }
| _         { error lexbuf 
                "found '%s' - don't know how to handle" @@ get lexbuf }

{
    let escape str = escape (B.create 100) (L.from_string str)

let to_string = function
| STR(str)          -> sprintf "STR(%s)" (escape str)
| INT(d)            -> sprintf "INT(%d)" d
| PLUS              -> sprintf "PLUS"
| MINUS             -> sprintf "MINUS"
| STAR              -> sprintf "STAR"
| SLASH             -> sprintf "SLASH"
| PLUSEQ            -> sprintf "PLUSEQ"
| MINUSEQ           -> sprintf "MINUSEQ"
| STAREQ            -> sprintf "STAREQ"
| SLASHEQ           -> sprintf "SLASHEQ"
| ID(str)           -> sprintf "ID(%s)" str
| ASSIGN            -> sprintf "ASSIGN"
| EOF               -> sprintf "EOF"

let main () =
    let lexbuf = set_filename "stdin" @@ L.from_channel stdin in
    let rec loop acc =  function
        | EOF   ->  to_string EOF :: acc |> List.rev
        | x     ->  loop (to_string x :: acc) (token lexbuf)
    in
        loop [] (token lexbuf) 
        |> String.concat " " 
        |> print_endline

    let () = main () (* call main function on startup *)
}

{
    (* open modules for lexing *)
    module L = Lexing
    module B = Buffer
    
    let get = L.lexeme
    let sprintf = Printf.sprintf

    type token = 
        | TYPE of string
        | VAR of string
        | FUNCTION of string
        | BREAK of string
        | OF of string
        | END of string
        | IN of string
        | NIL of string
        | LET of string
        | DO of string
        | TO of string
        | FOR of string
        | WHILE of string
        | ELSE of string
        | THEN of string
        | IF of string
        | ARRAY of string
        | ASSIGN of string
        | OR of string
        | AND of string
        | GE of string
        | GT of string
        | LE of string
        | LT of string
        | NEQ of string
        | EQ of string
        | DIVIDE of string
        | TIMES of string
        | MINUS of string
        | PLUS of string
        | EOF
    
    (* Utility for printing current position of buffer *)
    let position lexbuf =
        let p = 
            lexbuf.L.lex_curr_p in
                sprintf "%s:%d:%d" 
                p.L.pos_fname p.L.pos_lnum (p.L.pos_cnum - p.L.pos_bol)

    let set_filename (fname:string) (lexbuf:L.lexbuf) = 
        (lexbuf.L.lex_curr_p <-
            { lexbuf.L.lex_curr_p with L.pos_fname = fname }
        ; lexbuf
        )



    (* Utility for printing error message in buffer *)
    exception Error of string
    let error lexbuf fmt = 
        Printf.kprintf(fun msg ->
            raise(Error((position lexbuf)^" "^msg))) fmt
}

    (* Definition for whitespace delimiters *)
    let ws    = [' ' '\t']
    let nl    = ['\n']
    
    (* Definition for alphabetical characters *)
    let digit = ['0'-'9']
    let alpha = ['a'-'z' 'A'-'Z']
    let id    = alpha (alpha|digit)*

    rule token = parse
    | ws+       { token lexbuf  }
    | nl        { L.new_line lexbuf; token lexbuf  }
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
    | eof       { EOF           }
    | _         { error lexbuf 
                   "found '%s' - don't know how to handle" @@ get lexbuf }

    and escape b = parse
    | '&'       { B.add_string b "&amp;";  escape b lexbuf } 
    | '"'       { B.add_string b "&quot;"; escape b lexbuf } 
    | '\''      { B.add_string b "&apos;"; escape b lexbuf }
    | '>'       { B.add_string b "&gt;";   escape b lexbuf }
    | '<'       { B.add_string b "&lt;";   escape b lexbuf }
    | [^'&' '"' '\'' '>' '<']+ 
                { B.add_string b @@ get lexbuf
                            ; escape b lexbuf
                                        }
                | eof       { let x = B.contents b in B.clear b; x }
                | _         { error lexbuf 
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
    | _         { error lexbuf "found '%s' - don't know how to handle" @@ get lexbuf }

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
        let rec loop acc = function
            | EOF -> to_string EOF :: acc |> List.rev
            | x   -> loop (to_string x :: acc) (token lexbuf)
        in
            loop [] (token lexbuf)
            |> String.concat ""
            |> print endline

    let () = main ()
}

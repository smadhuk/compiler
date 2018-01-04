{
    (* open modules for lexing *)
    module L = Lexing
    module B = Buffer
    
    let get = L.lexeme
    let sprintf = Printf.sprintf

    type token = 
        | TYPE 
        | VAR 
        | FUNCTION 
        | BREAK 
        | OF 
        | END 
        | IN 
        | NIL 
        | LET
        | DO 
        | TO 
        | FOR 
        | WHILE 
        | ELSE 
        | THEN 
        | IF 
        | ARRAY 
        | ASSIGN
        | OR 
        | AND 
        | GE 
        | LE 
        | NEQ 
        | EQ 
        | AMP
        | LPAREN   
        | RPAREN   
        | LBRACKET 
        | RBRACKET 
        | LCURLY   
        | RCURLY   
        | LG       
        | LT       
        | GT       
        | LEQ      
        | GEQ      
        | BAR      
        | COMMA    
        | COLON    
        | SEMICOLON
        | DIVIDE
        | TIMES 
        | MINUS 
        | PLUS 
        | SLASH
        | STAR
        | PLUSEQ
        | MINUSEQ
        | STAREQ
        | SLASHEQ
        | INT of int
        | ID of string
        | STR of string
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
    | "while"   { WHILE         }
    | "for"     { FOR           }
    | "to"      { TO            }
    | "break"   { BREAK         }
    | "in"      { IN            }
    | "end"     { END           }
    | "function" { FUNCTION     }
    | "var"     { VAR           }
    | "let"     { LET           }
    | "type"    { TYPE          }
    | "array"   { ARRAY         }
    | "if"      { IF            }
    | "then"    { THEN          }
    | "else"    { ELSE          }
    | "of"      { OF            }
    | "do"      { DO            }
    | "nil"     { NIL           }
    | id        { ID(get lexbuf)}
    | '='       { EQ            }
    | '+'       { PLUS          }
    | '-'       { MINUS         }
    | '*'       { STAR          }
    | '/'       { SLASH         }
    | "+="      { PLUSEQ        }
    | "-="      { MINUSEQ       }
    | "*="      { STAREQ        }
    | "/="      { SLASHEQ       }
    | ":="      { ASSIGN        }
    | '&'       { AMP           }
    | '('       { LPAREN        }
    | ')'       { RPAREN        }
    | '['       { LBRACKET      }
    | ']'       { RBRACKET      }
    | '{'       { LCURLY        }
    | '}'       { RCURLY        }
    | "<>"      { LG            }
    | '<'       { LT            }
    | '>'       { GT            }
    | "<="      { LEQ           }
    | ">="      { GEQ           }
    | '|'       { BAR           }
    | ','       { COMMA         }
    | ':'       { COLON         }
    | ';'       { SEMICOLON     }
    | '"'       { STR (string (B.create 100) lexbuf) } (* see below *)
    | eof       { EOF           }
    | _         { error lexbuf 
                   "found '%s' - don't know how to handle" @@ get lexbuf }

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
    let string str = string (B.create 100) (L.from_string str)

    let to_string = function
    | STR(str)          -> sprintf "STR(%s)" (string str)
    | INT(d)            -> sprintf "INT(%d)" d
    | ID(str)           -> sprintf "ID(%s)" str
    | WHILE             -> sprintf "WHILE"
    | TYPE              -> sprintf "TYPE"
    | AND               -> sprintf "AND"
    | OR                -> sprintf "OR"
    | FOR               -> sprintf "FOR"
    | TO                -> sprintf "TO"
    | BREAK             -> sprintf "BREAK"
    | LET               -> sprintf "LET"
    | IN                -> sprintf "IN"
    | END               -> sprintf "END"
    | FUNCTION          -> sprintf "FUNCTION"
    | VAR               -> sprintf "VAR"
    | ARRAY             -> sprintf "ARRAY"
    | IF                -> sprintf "IF"
    | THEN              -> sprintf "THEN"
    | ELSE              -> sprintf "ELSE"
    | DO                -> sprintf "DO"
    | OF                -> sprintf "OF"
    | NIL               -> sprintf "NIL"
    | PLUS              -> sprintf "PLUS"
    | MINUS             -> sprintf "MINUS"
    | STAR              -> sprintf "STAR"
    | SLASH             -> sprintf "SLASH"
    | PLUSEQ            -> sprintf "PLUSEQ"
    | MINUSEQ           -> sprintf "MINUSEQ"
    | STAREQ            -> sprintf "STAREQ"
    | SLASHEQ           -> sprintf "SLASHEQ"
    | ASSIGN            -> sprintf "ASSIGN"
    | EOF               -> sprintf "EOF"
    | LPAREN            -> sprintf "LPAREN"
    | RPAREN            -> sprintf "RPAREN"
    | EQ                -> sprintf "EQ"
    | GE                -> sprintf "GE"
    | LE                -> sprintf "LE"
    | NEQ               -> sprintf "NEQ"
    | AMP               -> sprintf "AMP"
    | LBRACKET          -> sprintf "LBRACKET"
    | RBRACKET          -> sprintf "RBRACKET"
    | LCURLY            -> sprintf "LCURLY"
    | RCURLY            -> sprintf "RCURLY"
    | LG                -> sprintf "LG"
    | LT                -> sprintf "LT"
    | GT                -> sprintf "GT"
    | LEQ               -> sprintf "LEQ"
    | GEQ               -> sprintf "GEQ"
    | BAR               -> sprintf "BAR"
    | COMMA             -> sprintf  "COMMA"
    | COLON             -> sprintf  "COLON"
    | SEMICOLON         -> sprintf  "SEMICOLON"
    | DIVIDE            -> sprintf  "DIVIDE"
    | TIMES             -> sprintf  "TIMES"
    
    let main () = 
        let lexbuf = set_filename "stdin" @@ L.from_channel stdin in
        let rec loop acc = function
            | EOF -> to_string EOF :: acc |> List.rev
            | x   -> loop (to_string x :: acc) (token lexbuf)
        in
            loop [] (token lexbuf)
            |> String.concat " "
            |> print_endline

    let () = main ()
}

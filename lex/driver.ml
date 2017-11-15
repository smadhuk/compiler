module Parse = struct
    let parse filename =
        let file = open_in filename in
        let get _ = in_channel_length file in 
        let lexer = Lexing.from_channel get in
        let do_it () =
            let t = lexer() in
            print t; print "\n";
            if substring(t,0,3)="EOF" then () else do_it()
        close_out file

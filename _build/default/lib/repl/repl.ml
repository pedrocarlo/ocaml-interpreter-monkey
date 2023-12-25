open Core

let start in_chan out_chan =
  let rec loop () =
    fprintf out_chan ">> ";
    Out_channel.flush out_chan;
    let scanned = In_channel.input_line in_chan in
    match scanned with
    | None -> ()
    | Some line -> (
        match line with
        | "" -> loop ()
        | _ ->
            Lexer.create_lexer line |> Lexer.read_tokenizer
            |> List.iter ~f:(fun e ->
                   printf "Token: %s\n" @@ Lexer.token_to_str e);
            Out_channel.flush out_chan;
            loop ())
  in
  loop ()

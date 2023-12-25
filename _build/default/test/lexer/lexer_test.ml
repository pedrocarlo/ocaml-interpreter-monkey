open Lexer
open Core

let run_test test input =
  let l = Lexer.create_lexer input in
  let rec loop (test : token list) accum =
    match test with
    | [] -> accum
    | expected_tok :: tl ->
        let tok = Lexer.next_token l in
        (* if not @@ phys_equal tok hd then *)
        (* printf "expected %s got %s\n" (token_to_str hd) @@ token_to_str tok; *)
        loop tl @@ ((expected_tok, tok) :: accum)
  in
  loop test [] |> List.rev

let input1 = {|=+(){},;|}
let test1 = [ Assign; Plus; LParen; RParen; LBrace; RBrace; Comma; Semicolon ]

let%test_unit "simple tokens" =
  run_test test1 input1
  |> List.iter ~f:(fun (expected, tok) ->
         let expected = token_to_str expected in
         [%test_eq: string] expected (token_to_str tok))

let input2 =
  {|let five = 5;
let ten = 10;
   let add = fn(x, y) {
     x + y;
};
   let result = add(five, ten);|}

let test2 =
  [
    Let;
    Ident "five";
    Assign;
    Int 5;
    Semicolon;
    Let;
    Ident "ten";
    Assign;
    Int 10;
    Semicolon;
    Let;
    Ident "add";
    Assign;
    Function;
    LParen;
    Ident "x";
    Comma;
    Ident "y";
    RParen;
    LBrace;
    Ident "x";
    Plus;
    Ident "y";
    Semicolon;
    RBrace;
    Semicolon;
    Let;
    Ident "result";
    Assign;
    Ident "add";
    LParen;
    Ident "five";
    Comma;
    Ident "ten";
    RParen;
    Semicolon;
    EOF;
  ]

let%test_unit "keywords and identifiers added" =
  run_test test2 input2
  |> List.iter ~f:(fun (expected, tok) ->
         let expected = token_to_str expected in
         (* printf "Expected: %s Got: %s\n" expected (token_to_str tok); *)
         [%test_eq: string] expected (token_to_str tok))

let input3 =
  {|let five = 5;
  let ten = 10;
     let add = fn(x, y) {
       x + y;
  };
     let result = add(five, ten);
     !-/*5;
     5 < 10 > 5;
     if (5 < 10) {
         return true;
     } else {
         return false;
  }
  10 == 10; 10 != 9;|}

let test3 =
  [
    Let;
    Ident "five";
    Assign;
    Int 5;
    Semicolon;
    Let;
    Ident "ten";
    Assign;
    Int 10;
    Semicolon;
    Let;
    Ident "add";
    Assign;
    Function;
    LParen;
    Ident "x";
    Comma;
    Ident "y";
    RParen;
    LBrace;
    Ident "x";
    Plus;
    Ident "y";
    Semicolon;
    RBrace;
    Semicolon;
    Let;
    Ident "result";
    Assign;
    Ident "add";
    LParen;
    Ident "five";
    Comma;
    Ident "ten";
    RParen;
    Semicolon;
    Bang;
    Minus;
    Slash;
    Asterisk;
    Int 5;
    Semicolon;
    Int 5;
    Lt;
    Int 10;
    Gt;
    Int 5;
    Semicolon;
    If;
    LParen;
    Int 5;
    Lt;
    Int 10;
    RParen;
    LBrace;
    Return;
    True;
    Semicolon;
    RBrace;
    Else;
    LBrace;
    Return;
    False;
    Semicolon;
    RBrace;
    Int 10;
    Eq;
    Int 10;
    Semicolon;
    Int 10;
    Not_Eq;
    Int 9;
    Semicolon;
    EOF;
  ]

let%test_unit "more operators and keywords added" =
  run_test test3 input3
  |> List.iter ~f:(fun (expected, tok) ->
         let expected = token_to_str expected in
         (* printf "Expected: %s Got: %s\n" expected (token_to_str tok); *)
         [%test_eq: string] expected (token_to_str tok))

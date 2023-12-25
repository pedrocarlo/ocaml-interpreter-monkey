open Core

type lexer = {
  input : string;
  mutable position : int;
  mutable readPosition : int;
  mutable currChar : char option;
}

type operator = Eq | Plus

type token =
  | Illegal of string
  | EOF
  | Ident of string
  | Int of int
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | Lt
  | Gt
  | Eq
  | Not_Eq
  (* Delimiters *)
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  (* Keywords *)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return

let token_to_str tok =
  match tok with
  | Illegal x -> "ILLEGAL " ^ x
  | EOF -> "EOF"
  | Ident x -> x
  | Int x -> string_of_int x
  | Assign -> "="
  | Plus -> "+"
  | Minus -> "-"
  | Bang -> "!"
  | Asterisk -> "*"
  | Slash -> "/"
  | Lt -> "<"
  | Gt -> ">"
  | Eq -> "=="
  | Not_Eq -> "!="
  | Comma -> ","
  | Semicolon -> ";"
  | LParen -> "("
  | RParen -> ")"
  | LBrace -> "{"
  | RBrace -> "}"
  | Function -> "fn"
  | Let -> "let"
  | True -> "true"
  | False -> "false"
  | If -> "if"
  | Else -> "else"
  | Return -> "return"

let keywords =
  let m = Hashtbl.create (module String) in
  let _ = Hashtbl.add m ~key:"fn" ~data:Function in
  let _ = Hashtbl.add m ~key:"let" ~data:Let in
  let _ = Hashtbl.add m ~key:"true" ~data:True in
  let _ = Hashtbl.add m ~key:"false" ~data:False in
  let _ = Hashtbl.add m ~key:"if" ~data:If in
  let _ = Hashtbl.add m ~key:"else" ~data:Else in
  let _ = Hashtbl.add m ~key:"return" ~data:Return in
  m

let read_char l =
  if l.readPosition >= String.length l.input then l.currChar <- None
  else l.currChar <- Some (String.get l.input l.readPosition);
  l.position <- l.readPosition;
  l.readPosition <- l.readPosition + 1

let peek_char l =
  if l.readPosition >= String.length l.input then None
  else Some (String.get l.input l.readPosition)

let create_lexer input =
  let l = { input; position = 0; readPosition = 0; currChar = None } in
  read_char l;
  l

let lookup_ident x =
  match Hashtbl.find keywords x with None -> Ident x | Some y -> y

let is_letter c =
  match c with None -> false | Some x -> Char.is_alpha x || Char.equal x '_'

let read_identifier l =
  let pos = l.position in
  let rec loop () =
    if is_letter l.currChar then (
      read_char l;
      loop ())
  in
  loop ();
  String.slice l.input pos l.position

let is_digit c = match c with None -> false | Some x -> Char.is_digit x

let read_number l =
  let pos = l.position in
  let rec loop () =
    if is_digit l.currChar then (
      read_char l;
      loop ())
  in
  loop ();
  String.slice l.input pos l.position |> int_of_string

let skip_whitespace l =
  let rec loop l =
    match l.currChar with
    | None -> ()
    | Some c ->
        if Char.is_whitespace c then (
          read_char l;
          loop l)
        else ()
  in
  loop l

(* For now do everything here maybe later do a string/char to token function *)
let next_token l =
  let return_token r =
    skip_whitespace l;
    let res =
      match l.currChar with
      | None -> EOF
      | Some x -> (
          match x with
          | '=' -> (
              match peek_char l with
              | None -> Assign
              | Some y ->
                  if Char.equal y '=' then (
                    read_char l;
                    Eq)
                  else Assign)
          | ';' -> Semicolon
          | '(' -> LParen
          | ')' -> RParen
          | ',' -> Comma
          | '+' -> Plus
          | '{' -> LBrace
          | '}' -> RBrace
          | '-' -> Minus
          | '!' -> (
              match peek_char l with
              | None -> Bang
              | Some y ->
                  if Char.equal y '=' then (
                    read_char l;
                    Not_Eq)
                  else Bang)
          | '*' -> Asterisk
          | '/' -> Slash
          | '<' -> Lt
          | '>' -> Gt
          | x ->
              if is_letter l.currChar then
                let y = read_identifier l in
                r.return @@ lookup_ident y
              else if is_digit l.currChar then r.return @@ Int (read_number l)
              else Illegal (Char.to_string x))
    in
    read_char l;
    res
  in
  with_return return_token

let rec read_tokenizer l =
  let tok = next_token l in
  match tok with EOF -> [ EOF ] | _ -> tok :: read_tokenizer l

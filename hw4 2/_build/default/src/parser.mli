
(* The type of tokens. *)

type token = 
  | UNPAIR
  | TIMES
  | THEN
  | SND
  | SETREF
  | SET
  | SEMICOLON
  | RPAREN
  | RBRACE
  | PROC
  | PLUS
  | PAIR
  | NEWREF
  | MINUS
  | LPAREN
  | LETREC
  | LET
  | LESSTHAN
  | LBRACE
  | ISZERO
  | ISNUMBER
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | GREATERTHAN
  | FST
  | EQUALSMUTABLE
  | EQUALS
  | EOF
  | END
  | ELSE
  | DOT
  | DIVIDED
  | DEREF
  | DEBUG
  | COMMA
  | BEGIN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)

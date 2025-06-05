module type LANG = sig
  type token
  type nt
  type cst =
  | NT of (nt * cst list)
  | T of token
  val eof: token
  val token_match : token -> token -> bool
  val sample_code : token list
  val string_of_token : token -> string
  val string_of_nt : nt -> string
  val string_of_cst : cst -> string
end

module Lexer(Lang:LANG)=
  struct
    exception Lex_Error of string
    let tokens : Lang.token list ref = ref []
    let top () : Lang.token =
      match !tokens with
      | [] -> Lang.eof
      | h::_ -> h
    let pop (tk:Lang.token) : Lang.cst =
      match !tokens with
      | [] when Lang.token_match Lang.eof tk -> Lang.T Lang.eof
      | h::t when Lang.token_match h tk -> tokens := t; Lang.T h
      | _ -> raise (Lex_Error "Lex_error: next token is unexpected.")
    let reset (ts:Lang.token list) = tokens := ts
  end

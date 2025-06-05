module LR0Lang = 
  struct
    type token =
    | NUM of int
    | PLUS
    | EOF
    type nt =
    | Start
    | Expr
    type cst =
    | NT of (nt * cst list)
    | T of token

    let eof = EOF

    (* 2 + 3 + 4 *)
    let sample_code = [NUM 2; PLUS; NUM 3; PLUS; NUM 4; EOF]

    let token_match t1 t2 =
      match t1,t2 with
      | NUM _, NUM _ -> true
      | _ -> t1 = t2
    let string_of_token : token -> string = function
    | NUM n -> "NUM(" ^ string_of_int n ^ ")"
    | PLUS -> "PLUS"
    | EOF -> "EOF"
    let string_of_nt : nt -> string = function
    | Start -> "Start"
    | Expr -> "Expr"
    let rec string_of_cst : cst -> string = function
    | NT (nt, csts) 
      -> "(" ^ string_of_nt nt ^ ", [" ^ 
        (List.map (string_of_cst) csts |> String.concat ";") ^
        "])"
    | T t -> string_of_token t
  end

module LR0Parser =
  struct
    exception Parse_error of string
    open LR0Lang
    open Lexer.Lexer(LR0Lang)

    let rec back (state:int list) (reads:cst list) (len:int): int list * cst list * cst list =
      if len = 0 then (state,reads,[])
      else match state,reads with
      | _::st,cst::rt -> let (st',rt',csts') = back st rt (len-1) in (st',rt',cst::csts')
      | _ -> raise (Parse_error "Parse error: over reduce")



    let reduce (state:int list) (reads:cst list) (rule:int) : int list * cst list =
      let (st,rt,cst) =
        (match rule with
        | 1 -> let (st,rt,csts) = back state reads 2 in (st,rt,NT(Start,List.rev csts))
        | 2 -> let (st,rt,csts) = back state reads 3 in (st,rt,NT(Expr,List.rev csts))
        | 3 -> let (st,rt,csts) = back state reads 1 in (st,rt,NT(Expr,List.rev csts))
        | _ -> raise (Parse_error "Parse error: reduce by unexpected rule_id"))
      in
      match st with
      | [] -> raise (Parse_error "Parse error:lost state")
      | s::_ -> 
      match s, cst with
      | 1, NT(Expr,_) -> (3::st,cst::rt)
      | _ -> raise (Parse_error "Parse error: fail to shift after redeced")

    let rec step (state:int list) (reads:cst list) =
      match state with
      | [] -> raise (Parse_error "Parse error:lost state")
      | s::_ -> 
      match s, top() with
      | 3,EOF -> NT(Start,List.rev (T EOF:: reads))
      | 2,_ -> let (st,rt) = reduce state reads 3 in step st rt
      | 5,_ -> let (st,rt) = reduce state reads 2 in step st rt
      | 1,NUM _ -> step (2::state) (pop(NUM 0)::reads)
      | 3,PLUS -> step (4::state) (pop(PLUS)::reads)
      | 4,NUM _ -> step (5::state) (pop(NUM 0)::reads)
      | _,t -> raise (Parse_error ("Parse_error : unexpexted token "^ string_of_token t))

    let main (ts:token list) = 
      reset ts;
      step [1] []

  end
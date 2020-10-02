module Pricing =
    module Expr =
        type e_bool =
            | True
            | False
            | Not of e_bool
            | Or of e_bool * e_bool
            | And of e_bool * e_bool
            | NumEq of e_num * e_num
            | NumGt of e_num * e_num
        and e_num =
            | Result
            | Const of float
            | Add of e_num * e_num
            | Sub of e_num * e_num
            | Mul of e_num * e_num
            | Div of e_num * e_num
            | If of e_bool * e_num * e_num
            | Call of (float -> float) * e_num // Should probably take more args?

        let rec eval_bool result = function
            | True -> true
            | False -> false
            | Not e -> not (eval_bool result e)
            | Or (e1, e2) ->
                eval_bool result e1 || eval_bool result e2
            | And (e1, e2) ->
                eval_bool result e1 && eval_bool result e2
            | NumEq (e1, e2) ->
                eval_num result e1 = eval_num result e2
            | NumGt (e1, e2) ->
                eval_num result e1 > eval_num result e2
        and eval_num result = function
            | Result -> result
            | Const n -> n
            | Add (e1, e2) ->
                eval_num result e1 + eval_num result e2
            | Sub (e1, e2) ->
                eval_num result e1 - eval_num result e2
            | Mul (e1, e2) ->
                eval_num result e1 * eval_num result e2
            | Div (e1, e2) ->
                eval_num result e1 / eval_num result e2
            | If (cond, e1, e2) ->
                if eval_bool result cond then eval_num result e1 else eval_num result e2
            | Call (f, e) ->
                f (eval_num result e)

   // Test it in the top-level:
   // open Expr
   // assert (eval_num 4 (Mul (Result, Const 2)) = 8)

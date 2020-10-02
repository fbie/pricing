module Pricing =
    module Expr =
        type e_bool =
            | True
            | False
            | Not of e_bool
            | Or of e_bool * e_bool
            | And of e_bool * e_bool
            | Eq of e_num * e_num
            | Gt of e_num * e_num
        and e_num =
            | Result
            | Const of float
            | Add of e_num * e_num
            | Sub of e_num * e_num
            | Mul of e_num * e_num
            | Div of e_num * e_num
            | If of e_bool * e_num * e_num
            | Call1 of (float -> float) * e_num
            | Call2 of (float -> float -> float) * e_num * e_num

        let rec eval_bool result = function
            | True -> true
            | False -> false
            | Not e -> not (eval_bool result e)
            | Or (e1, e2) ->
                eval_bool result e1 || eval_bool result e2
            | And (e1, e2) ->
                eval_bool result e1 && eval_bool result e2
            | Eq (e1, e2) ->
                eval_num result e1 = eval_num result e2
            | Gt (e1, e2) ->
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
            | Call1 (f, e) ->
                f (eval_num result e)
            | Call2 (f, e1, e2) ->
                f (eval_num result e1) (eval_num result e2)

    module Embedded =
        open Expr
        open System

        // Redefining these gives a bunch of warnings.
        let x = Result
        let cst n = Const n
        let (+) e1 e2 = Add (e1, e2)
        let (-) e1 e2 = Sub (e1, e2)
        let (*) e1 e2 = Mul (e1, e2)
        let (/) e1 e2 = Div (e1, e2)
        let iff cond e1 e2 = If (cond, e1, e2)

        let (!) e = Not e
        let (||) e1 e2 = Or (e1, e2)
        let (&&) e1 e2 = And (e1, e2)
        let (=) e1 e2 = Eq (e1, e2)
        let (>) e1 e2 = Gt (e1, e2)
        let (>=) e1 e2 = (e1 = e2) || (e1 > e2)
        let (<) e1 e2 = !((e1 = e2) || (e1 > e2)) // Syntactic sugar.
        let (<=) e1 e2 = (e1 = e2) || (e1 < e2)

        let private call1 f e = Call1 (f, e)

        let sqrt = call1 Math.Sqrt

        let private call2 f e1 e2 = Call2 (f, e1, e2)
        let private curry f a b = f (a, b)

        let log = call2 (curry Math.Log)
        let pow = call2 (curry Math.Pow)

        let test () =
            // Operators precedence is maintained.
            eval_num 1.0 ((cst 2.0) * (cst 3.0) + x)

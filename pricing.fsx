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
            | Time
            | Result
            | Const of float
            | Add of e_num * e_num
            | Sub of e_num * e_num
            | Mul of e_num * e_num
            | Div of e_num * e_num
            | If of e_bool * e_num * e_num
            | Call1 of (float -> float) * e_num
            | Call2 of (float -> float -> float) * e_num * e_num

        let rec private mix_bool t r = function
            | True -> True
            | False -> False
            | Not e ->
                match mix_bool t r e with
                    | True -> False
                    | False -> True
                    | e -> Not e
            | And (e1, e2) ->
                match mix_bool t r e1 with
                    | True -> mix_bool t r e2
                    | False -> False
                    | e1 -> And (e1, mix_bool t r e2)
            | Or (e1, e2) ->
                match mix_bool t r e1 with
                    | True -> True
                    | False -> mix_bool t r e2
                    | e1 -> Or (e1, mix_bool t r e2)
            | Eq (e1, e2) ->
                match mix_num t r e1, mix_num t r e2 with
                    | Const x, Const y -> if x = y then True else False
                    | e1, e2 -> Eq (e1, e2)
            | Gt (e1, e2) ->
                match mix_num t r e1, mix_num t r e2 with
                    | Const x, Const y -> if x > y then True else False
                    | e1, e2 -> Gt (e1, e2)
        and private mix_num t r = function
            | Time -> t
            | Result -> r
            | Const n as cst -> cst
            | Add (e1, e2) ->
                match mix_num t r e1, mix_num t r e2 with
                    | Const x, Const y -> Const (x - y)
                    | Const 0.0, e2 -> e2
                    | e1, Const 0.0 -> e1
                    | e1, e2 -> Add (e1, e2)
            | Sub (e1, e2) ->
                match mix_num t r e1, mix_num t r e2 with
                    | Const x, Const y -> Const (x - y)
                    | e, Const 0.0 -> e
                    | e1, e2 -> Sub (e1, e2)
            | Mul (e1, e2) ->
                match mix_num t r e1, mix_num t r e2 with
                    | Const x, Const y -> Const (x * y)
                    | Const 0.0, _ | _, Const 0.0 -> Const 0.0
                    | Const 1.0, e -> e
                    | e, Const 1.0 -> e
                    | e1, e2 -> Mul (e1, e2)
            | Div (e1, e2) ->
                match mix_num t r e1, mix_num t r e2 with
                    | Const x, Const y -> Const (x / y)
                    | Const 0.0, _ -> Const 0.0
                    | _, Const 0.0 -> failwith "Division by zero"
                    | e, Const 1.0 -> e
                    | e1, e2 -> Div (e1, e2)
            | If (cond, e1, e2) ->
                match mix_bool t r cond with
                    | True -> mix_num t r e1
                    | False -> mix_num t r e2
                    | cond -> If (cond, mix_num t r e1, mix_num t r e2)
            | Call1 (f, e) ->
               match mix_num t r e with
                    | Const c -> Const (f c)
                    | e -> Call1 (f, e)
            | Call2 (f, e1, e2) ->
                match mix_num t r e1, mix_num t r e2 with
                    | Const c1, Const c2 -> Const (f c1 c2)
                    | Const c1, e -> Call1 (f c1, e)
                    | e1, e2 -> Call2 (f, e1, e2)

        let reduce =
            mix_num Time Result

        let mix t =
            mix_num (Const t) Result

        let eval t r =
            mix_num (Const t) (Const r)

        let rec private invert_bool = function
            | Not e -> Not (invert_bool e)
            | Or (e1, e2) ->
                Or (invert_bool e1, invert_bool e2)
            | And (e1, e2) ->
                And (invert_bool e1, invert_bool e2)
            | Eq (e1, e2) ->
                Eq (invert_num e1, invert_num e2)
            | Gt (e1, e2) ->
                Gt (invert_num e1, invert_num e2)
            | True -> True
            | False -> False
        and private invert_num = function
            | Time -> Time
            | Result -> Result
            | Const _ as c -> c
            | Add (e1, e2) ->
                Sub (invert_num e1, invert_num e2)
            | Sub (e1, e2) ->
                Add (invert_num e1, invert_num e2)
            | Mul (e1, e2) ->
                Div (invert_num e1, invert_num e2)
            | Div (e1, e2) ->
                Mul (invert_num e1, invert_num e2)
            | If (cond, e1, e2) ->
                If (invert_bool cond, invert_num e1, invert_num e2)
            | Call1 (f, e) ->
                Call1 (f, invert_num e)
            | Call2 (f, e1, e2) ->
                Call2 (f, invert_num e1, invert_num e2)

        let invert = invert_num

    module Embedded =
        open Expr
        open System

        // Redefining these gives a bunch of warnings.
        let x = Result
        let t = Time
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

        let e = (cst 2.0) * (cst 3.0) + (iff (t > (cst 0.0)) (x - t) (x - (cst 1.0)))
        let test () = eval 1.0 1.0 e

datatype Sexp = Nil
              | Symbol of string
              | Number of int
              | Pair of (Sexp * Sexp);

fun cons car cdr = Pair (car, cdr);

fun repr Nil = "()"
  | repr (Symbol x) = x
  | repr (Number x) = (Int.toString x)
  | repr (Pair (car, cdr)) = "(" ^ (repr car) ^ (reprTail cdr)
and reprTail Nil = ")"
  | reprTail (Pair (car, cdr)) = " " ^ (repr car) ^ (reprTail cdr)
  | reprTail (x: Sexp) = " . " ^ (repr x) ^ ")";

fun iota n =
    let
        fun build 0 acc = acc
          | build i acc = build (i - 1) (cons (Number i) acc);
    in
        build n Nil
    end;

repr (cons (Number 1) (cons (Number 2) (cons (Number 3) Nil)));
repr (iota 5);

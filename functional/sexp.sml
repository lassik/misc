datatype Sexp = Nil
              | Symbol of string
              | Number of int
              | Pair of (Sexp * Sexp);

fun cons car cdr = Pair (car, cdr);

fun repr (x: Sexp) =
    let

        fun reprTail Nil = ")"
          | reprTail (Pair (car, cdr)) = " " ^ (reprOf car) ^ (reprTail cdr)
          | reprTail (x: Sexp) = " . " ^ (reprOf x) ^ ")"

        and reprOf Nil = "()"
          | reprOf (Symbol x) = x
          | reprOf (Number x) = (Int.toString x)
          | reprOf (Pair (car, cdr)) = "(" ^ (reprOf car) ^ (reprTail cdr)

    in
        reprOf x
    end;

fun iota n =
    let
        fun build 0 acc = acc
          | build i acc = build (i - 1) (cons (Number i) acc);
    in
        build n Nil
    end;

repr (cons (Number 1) (cons (Number 2) (cons (Number 3) Nil)));
repr (iota 5);

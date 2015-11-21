val cbs : (int -> unit) list ref = ref []

(* register the call back function *)
fun onKeyEvent f =
    cbs := f :: (!cbs)

(* handle the event *)
(* when a key is pressed, evaluate all the registered function*)
fun onEvent i =
    let fun loop fs =
            case fs of
                [] => ()
              | f::fs' => (f i; loop fs')
    in loop (!cbs) end

val timesPressed = ref 0

(* register function1 *)
val _ = onKeyEvent (fn _ => timesPressed := (!timesPressed) + 1)

(* only care about the cases where "i" is pressed *)
fun printIfPressed i =
    onKeyEvent (fn j => if i = j
                        then print ("you pressed " ^ Int.toString i ^ "\n")
                        else ())

(* register function2 *)
val _ = printIfPressed 4
(* register function3 *)
val _ = printIfPressed 11
(* register function4 *)
val _ = printIfPressed 23
(* register function5 *)
val _ = printIfPressed 4

(* when evaluate at this point, there are 5 functions registered *)

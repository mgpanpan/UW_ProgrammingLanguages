datatype set = S of {insert : int -> set, member : int -> bool, size : unit -> int, printset : unit -> unit}
                        
(*
val empty_set =
    let
        fun make_set xs =
            let
                fun contains i = List.exists (fn j => i=j) xs
            in
                S { insert = fn i => if contains i
                                     then make_set xs
                                     else make_set (i::xs),
                    member = contains,
                    size = fn () => length xs,
                    printset = fn () =>
                                  let fun iter xs =
                                          case xs of
                                              [] => print "\n"
                                            | x::xs' =>
                                              (print (Int.toString x);
                                               print " ";
                                               iter xs')
                                  in iter xs end
                }
            end
    in
        make_set []
    end
*)


val empty_set =
    let
        fun make_set xs =
            S { insert = fn i => if List.exists (fn j => i=j) xs
                                 then make_set xs
                                 else make_set (i::xs),
                member = fn i => List.exists (fn j => i=j) xs,
                size = fn () => length xs,
                printset = fn () =>
                              let fun iter xs =
                                      case xs of
                                          [] => print "\n"
                                        | x::xs' =>
                                          (print (Int.toString x);
                                           print " ";
                                           iter xs')
                              in iter xs end
              }
    in
        make_set []
    end

                        
(* mutable version 
val empty_set =
    let 
        val xs = ref []
    in
        S { insert = fn i => if List.exists (fn j => i=j) !xs
                             then empty_set
                             else (xs := i::!xs; empty_set),
            member = fn i => List.exists (fn j => i=j) !xs,
            size = fn () => length xs,
            printset = fn () =>
                          let fun iter xs =
                                  case xs of
                                      [] => print "\n"
                                    | x::xs' =>
                                      (print (Int.toString x);
                                       print " ";
                                       iter xs')
                          in iter xs end}
    end

*)
                        
val S set1 = empty_set
val test1 = (#member set1) 10
val _ = (#printset set1) ()
val S set2 = (#insert set1) 10
val test2 = (#member set2) 10
val _ = (#printset set2) ()
val S set3 = (#insert set2) 20
val S set4 = (#insert set3) 30
val S set5 = (#insert set4) 40
val S set6 = (#insert set5) 90
val S set7 = (#insert set6) 80
val _ = (#printset set7) ()
val set7_size = (#size set7) ()

fun use_sets () =
    let val S s1 = empty_set
        val S s2 = (#insert s1) 34
        val S s3 = (#insert s2) 34
        val S s4 = #insert s3 19
    in
        if (#member s4) 42
        then 99
        else if (#member s4) 19
        then 17 + (#size s3) ()
        else 0
    end

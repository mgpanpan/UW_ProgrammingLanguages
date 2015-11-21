use "my_list.sml";

val doubleAll = map (fn x => 2*x)
fun ntimesAll n = map (fn x => n * x)
                      
fun countNs (xs, n : int) = length (filter (fn x => x=n) xs)
                      
fun convert2list x =
    case x of
        Empty => []
      | Cons(xs, xs') => xs :: (convert2list xs')

val t1 = Cons(1, Cons(2, Cons(10, Cons(9, Cons(1, (Cons(3, Cons(1, Empty))))))))
             
val t2 = doubleAll t1
val t3 = ntimesAll 10 t1

val t4 = countNs (t1, 1)
val t5 = countNs (t2, 1)
val t6 = countNs (t2, 2)
                 
val t7 = convert2list t1
val t8 = convert2list t2
val t9 = convert2list t3

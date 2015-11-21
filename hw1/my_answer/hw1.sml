(* 1 *)
fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if (#1 date1) < (#1 date2)
    then true
    else if (#1 date1) = (#1 date2)
    then if (#2 date1) < (#2 date2)
         then true
         else if (#2 date1) = (#2 date2)
         then if (#3 date1) < (#3 date2)
              then true
              else false
         else false
    else false

(* 2 *)
fun number_in_month (datelist : (int * int * int) list, month : int) =
    if null datelist
    then 0
    else if (#2 (hd datelist)) = month
    then number_in_month(tl datelist, month) + 1
    else number_in_month(tl datelist, month)

(* 3 *)
fun number_in_months (datelist : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(datelist, hd months) +
         number_in_months(datelist, (tl months))

(* 4 *)
fun dates_in_month (datelist : (int * int * int) list, month : int) =
    if null datelist
    then []
    else if #2 (hd datelist) = month
    then hd datelist :: dates_in_month(tl datelist, month)
    else dates_in_month(tl datelist, month)

(* 5 *)
fun dates_in_months (datelist : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(datelist, hd months) @
         dates_in_months(datelist, tl months)

(* 6 *)
fun get_nth (strlist : string list, n : int) =
    if n = 1
    then hd strlist
    else get_nth (tl strlist, n-1)

(* 7 *)
val months = ["January","February","March","April","May","June",
              "July","August","September","October","November","December"]

fun date_to_string (date : (int * int * int)) =
    get_nth(months, #2 date) ^ " " ^
    Int.toString(#3 date) ^ ", " ^
    Int.toString(#1 date)

(* 8 *)
fun number_before_reaching_sum (sum : int, numlist : int list) =
    let
        fun sum_iter(numlist, num, sumlist) =
            let
                val sum_tmp = sumlist + (hd numlist)
            in
                if sum_tmp >= sum
                then num
                else sum_iter(tl numlist, num+1, sum_tmp)
            end
    in
        sum_iter(numlist, 0, 0)
    end
                
(* 9 *)
val daynum_of_month = [31,28,31,30,31,30,31,31,30,31,30,31]
fun what_month (day_of_year : int) =
    number_before_reaching_sum(day_of_year, daynum_of_month) + 1

(* 10 *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)

(* 11 *)
fun oldest(datelist : (int * int * int) list) =
    if null datelist
    then NONE
    else
        let
            fun oldest_unempty (datelist : (int * int * int) list) =
                if null (tl datelist)
                then hd datelist
                else
                    let val tl_oldest = oldest_unempty(tl datelist)
                    in
                        if is_older(hd datelist, tl_oldest)
                        then hd datelist
                        else tl_oldest
                    end
        in
            SOME (oldest_unempty datelist)
        end

(* 12 *)
(* help function : remove_duplicates *)
fun number_in_months_challenge (datelist : (int * int * int) list,
                                months : int list) =
    let fun remove_duplicates (xs : int list) =
            let fun is_inlist (x : int, xs : int list) =
                    if null xs
                    then false
                    else if x = (hd xs)
                    then true
                    else is_inlist(x, tl xs)
            in
                if null xs
                then []
                else if is_inlist(hd xs, tl xs)
                then remove_duplicates(tl xs)
                else (hd xs) :: remove_duplicates(tl xs)
            end
    in
        number_in_months(datelist, remove_duplicates(months))
    end
        
fun dates_in_months_challenge (datelist : (int * int * int) list,
                               months : int list) =
    let fun remove_duplicates (xs : int list) =
            let fun is_inlist (x : int, xs : int list) =
                    if null xs
                    then false
                    else if x = (hd xs)
                    then true
                    else is_inlist(x, tl xs)
            in
                if null xs
                then []
                else if is_inlist(hd xs, tl xs)
                then remove_duplicates(tl xs)
                else (hd xs) :: remove_duplicates(tl xs)
            end
    in
        dates_in_months(datelist, remove_duplicates(months))
    end
    
(* 13 *)
(* help function : is_leapyear *)
fun is_leapyear (year : int) =
    if year mod 400 = 0
    then true
    else if year mod 4 = 0 andalso year mod 100 <> 0
    then true
    else false

(* help function : return the nth element of the list *)
fun list_ref (xs : int list, n : int) =
    if n = 1
    then hd xs
    else list_ref(tl xs, n-1)
             
fun reasonable_date (date : int * int * int) =
    if #1 date <= 0
    then false
    else if #2 date < 1 orelse #2 date > 12
    then false
    else
        let
            val daynum_of_month = [31,28,31,30,31,30,31,31,30,31,30,31]
            val daynum = if is_leapyear(#1 date) andalso (#2 date = 2)
                         then list_ref(daynum_of_month, 2) + 1
                         else list_ref(daynum_of_month, #2 date)
        in
            if #3 date <1 orelse #3 date > daynum
            then false
            else true
        end

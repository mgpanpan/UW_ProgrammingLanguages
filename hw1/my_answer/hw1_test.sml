(* 1, test is_older *)
is_older((2020,1,3), (2020,1,4));
is_older((2020,10,3), (2020,10,3));
is_older((2020,12,10), (2020,12,9));
is_older((2020,1,3), (2030,1,3));
is_older((2020,1,3), (2020,10,3));

(* 2, test number_in_month *)
number_in_month([(2020,1,3),(2020,10,4),(2030,10,5),
                 (2040,11,5),(2040,10,12)], 10);
number_in_month([(2020,1,3),(2020,10,4),(2030,10,5),
                 (2040,11,5),(2040,10,12)], 1);
number_in_month([(2020,1,3),(2020,10,4),(2030,10,5),
                 (2040,11,5),(2040,10,12)], 11);

(* 3, test number_in_months *)
number_in_months([(2020,1,3),(2020,10,4),(2030,10,5),
                  (2040,11,5),(2040,10,12)], [1]);
number_in_months([(2020,1,3),(2020,10,4),(2030,10,5),
                  (2040,11,5),(2040,10,12)], [10]);
number_in_months([(2020,1,3),(2020,10,4),(2030,10,5),
                  (2040,11,5),(2040,10,12)], [11]);
number_in_months([(2020,1,3),(2020,10,4),(2030,10,5),
                  (2040,11,5),(2040,10,12)], [1,10]);
number_in_months([(2020,1,3),(2020,10,4),(2030,10,5),
                  (2040,11,5),(2040,10,12)], [1,10,11]);

(* 4, test dates_in_month *)
dates_in_month([(2020,1,3),(2020,10,4),(2030,10,5),
                (2040,11,5),(2040,10,12)], 1);
dates_in_month([(2020,1,3),(2020,10,4),(2030,10,5),
                (2040,11,5),(2040,10,12)], 2);
dates_in_month([(2020,1,3),(2020,10,4),(2030,10,5),
                (2040,11,5),(2040,10,12)], 3);
dates_in_month([(2020,1,3),(2020,10,4),(2030,10,5),
                (2040,11,5),(2040,10,12)], 10);
dates_in_month([(2020,1,3),(2020,10,4),(2030,10,5),
                (2040,11,5),(2040,10,12)], 11);

(* 5, test dates_in_month *)
dates_in_months([(2020,1,3),(2020,10,4),(2030,10,5),
                 (2040,11,5),(2040,10,12)], [1]);
dates_in_months([(2020,1,3),(2020,10,4),(2030,10,5),
                 (2040,11,5),(2040,10,12)], [10]);
dates_in_months([(2020,1,3),(2020,10,4),(2030,10,5),
                 (2040,11,5),(2040,10,12)], [11]);
dates_in_months([(2020,1,3),(2020,10,4),(2030,10,5),
                 (2040,11,5),(2040,10,12)], [1,10]);
dates_in_months([(2020,1,3),(2020,10,4),(2030,10,5),
                 (2040,11,5),(2040,10,12)], [1,10,11]);

(* 6, test get_nth *)
(* val months = ["January","February","March","April","May","June", *)
(*               "July","August","September","October","November","December"]; *)

get_nth(months, 1);
(* get_nth(months, 0); *)
(* get_nth(months, 13); *)
get_nth(months, 10);
get_nth(months, 12);

(* 7, test date_to_string *)
date_to_string((2020,1,3));

(* 8, test number_before_reaching_sum *)
number_before_reaching_sum(1, [1,2,3]);
number_before_reaching_sum(2, [1,2,3]);
number_before_reaching_sum(5, [1,2,3]);
number_before_reaching_sum(10, [1,2,3,4,5,6]);
number_before_reaching_sum(10, [1,2,3,3,5,6]);

(* 9, test what_month *)
what_month(1);
what_month(31);
what_month(32);
what_month(59);
what_month(60);
what_month(365);

(* 10, test month_range *)
val tmp1 = month_range(1,10);
length(tmp1);
val tmp2 = month_range(1,365);
length(tmp2);
val tmp3 = month_range(40,30);
length(tmp3);
val tmp4 = month_range(50, 100);
val tmp5 = month_range(32, 363);

(* 11, test oldest *)
oldest([(2020,1,3),(2020,10,4),(2030,10,5),
             (2040,11,5),(2040,10,12)]);
oldest([(2020,10,4),(2030,10,5),(2020,1,3),
             (2040,11,5),(2040,10,12)]);
oldest([(2030,10,5),(2040,11,5),(2020,1,3),
             (2020,10,4),(2040,10,12)]);

(* 12(1), test number_in_months_challenge*)
(* the second argument may have some same component *)
number_in_months_challenge([(2020,1,3),(2020,10,4),(2030,10,5),
                            (2040,11,5),(2040,10,12)], [1]);
number_in_months_challenge([(2020,1,3),(2020,10,4),(2030,10,5),
                            (2040,11,5),(2040,10,12)], [1,1,1,1,1]);
number_in_months_challenge([(2020,1,3),(2020,10,4),(2030,10,5),
                            (2040,11,5),(2040,10,12)], [1,10,1,10,11,11,1]);
number_in_months_challenge([(2020,1,3),(2020,10,4),(2030,10,5),
                            (2040,11,5),(2040,10,12)], [1,10,10,1]);
number_in_months_challenge([(2020,1,3),(2020,10,4),(2030,10,5),
                            (2040,11,5),(2040,10,12)], [1,10,11,11,10,1]);

(* 12(2), test dates_in_months_challenge*)
(* the second argument may have some same component *)
dates_in_months_challenge([(2020,1,3),(2020,10,4),(2030,10,5),
                            (2040,11,5),(2040,10,12)], [1]);
dates_in_months_challenge([(2020,1,3),(2020,10,4),(2030,10,5),
                            (2040,11,5),(2040,10,12)], [1,1,1,1,1]);
dates_in_months_challenge([(2020,1,3),(2020,10,4),(2030,10,5),
                            (2040,11,5),(2040,10,12)], [1,10,1,10,11,11,1]);
dates_in_months_challenge([(2020,1,3),(2020,10,4),(2030,10,5),
                            (2040,11,5),(2040,10,12)], [1,10,10,1]);
dates_in_months_challenge([(2020,1,3),(2020,10,4),(2030,10,5),
                            (2040,11,5),(2040,10,12)], [1,10,11,11,10,1]);

(* 13 *)
reasonable_date (2020, 1, 1);
reasonable_date (2020, 1,31);
reasonable_date (2020, 1,32);
reasonable_date (2020, 2,31);
reasonable_date (2020, 2,29);
reasonable_date (2020, 2,28);
reasonable_date (2020, 100,31);
reasonable_date (2020, 10,31);
reasonable_date (2020, 12,31);
reasonable_date (2000, 2,29);
reasonable_date (1900, 2,29);

(* 
Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.) 
*)

fun is_older (d1 : int*int*int, d2 : int*int*int) =
    if #1 d1 < #1 d2 then true 
    else if #1 d1 > #1 d2 then false  
    else if #2 d1 < #2 d2 then true  
    else if #2 d1 > #2 d2 then false  
    else if #3 d1 < #3 d2 then true  
    else false


(*
2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month. 
*)

fun number_in_month (dt : (int * int * int) list, mon : int) =
    if null dt
    then 0
    else (if #2 (hd dt) = mon then 1 else 0) + number_in_month (tl dt, mon)


(*
3. Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. 
*)

fun number_in_months (dt : (int * int * int) list, mon : int list) =
    if null mon
    then 0
    else (number_in_month (dt, hd mon)) + number_in_months(dt, tl mon)


(*
4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. 
*)

fun dates_in_month (dt : (int * int * int) list, mon : int) =
    if null dt
    then []
    else if #2 (hd dt) = mon then (hd dt) :: dates_in_month(tl dt, mon)    
    else dates_in_month(tl dt, mon)    
    

(*
5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@). 
*)

fun dates_in_months (dt : (int * int * int) list, mon : int list) =
    if null mon
    then []
    else (dates_in_month (dt, hd mon)) @ dates_in_months(dt, tl mon)


(*
6. Write a function get_nth that takes a list of strings and an int n and returns the n
th element of the
list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay. 
*)

fun get_nth (st : string list, n : int) = 
    if n = 1
    then hd st
    else get_nth (tl st, n-1)
    

(*
7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December.
*)

val mon = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

fun date_to_string (dt : (int * int * int)) =   
    get_nth(mon, #2 dt) ^ " " ^ Int.toString(#3 dt) ^ ", " ^ Int.toString(#1 dt)


(*
8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case.
*)

fun number_before_reaching_sum (sum : int, numbers : int list) =
    if sum <= hd numbers
    then 0
    else (1) + number_before_reaching_sum(sum - (hd numbers), (tl numbers))


(*
9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem.
*)

val days_in_months =  [31,28,31,30,31,30,31,31,30,31,30,31]

fun what_month (dayofyear : int) = 
    number_before_reaching_sum(dayofyear, days_in_months) + 1


(*
10. Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.
*)

fun month_range (day1 : int, day2 : int) = 
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)


(*
11. Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. 
*)

fun oldest (dt : (int * int * int) list) =
    if null dt
    then NONE
    else 
        let 
            val result = oldest(tl dt)
        in 
            if isSome result andalso is_older(valOf result, (hd dt))
            then result
            else SOME (hd dt)
        end

(*
fun new_is_older (d1 : int*int*int, d2 : int*int*int) =
    if #1 d1 < #1 d2 then d1
    else if #1 d1 > #1 d2 then d2 
    else if #2 d1 < #2 d2 then d1 
    else if #2 d1 > #2 d2 then d2  
    else if #3 d1 < #3 d2 then d2  
    else d2

fun oldest (dt : (int * int * int) list) =
    if null (tl dt)
    then hd dt  
    else new_is_older(hd dt, oldest(tl dt))  
*)

(*
12. Challenge Problem: Write functions number_in_months_challenge and dates_in_months_challenge
that are like your solutions to problems 3 and 5 except having a month in the second argument multiple
times has no more effect than having it once. (Hint: Remove duplicates, then use previous work.)
*)

fun in_list (num : int, mylist : int list) = 
    if null mylist
    then false
    else if num = (hd mylist) then true
    else in_list(num, tl mylist)


fun distinct_months (mon : int list, result : int list) =    
    if null (tl mon) 
    then (if in_list(hd mon, result) then result else (hd mon :: result))
    else if in_list(hd mon, result) then distinct_months(tl mon, result)
    else distinct_months(tl mon, (hd mon) :: result)


fun number_in_months_challenge (dt : (int * int * int) list, mon : int list) = 
    if null (tl mon)
    then number_in_months(dt, mon)
    else number_in_months(dt, distinct_months((tl mon), (hd mon :: [])))


fun dates_in_months_challenge (dt : (int * int * int) list, mon : int list) =
    if null (tl mon)
    then dates_in_months(dt, mon)
    else dates_in_months(dt, distinct_months((tl mon), (hd mon :: [])))


(*
13. Challenge Problem: Write a function reasonable_date that takes a date and determines if it
describes a real date in the common era. A “real date” has a positive year (year 0 did not exist), a
month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap
years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.
(Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.)
*)

fun is_leap_year (yr : int) =
    if yr mod 400 = 0  orelse ((yr mod 4 = 0) andalso (yr mod 100 <> 0)) 
    then true
    else false


fun days_in_months (yr : int) =
    if is_leap_year(yr) 
    then [31,29,31,30,31,30,31,31,30,31,30,31]
    else [31,28,31,30,31,30,31,31,30,31,30,31]

fun get_nth_int (st : int list, n : int) = 
    if n = 1
    then hd st
    else get_nth_int (tl st, n-1)


fun reasonable_date (dt : (int * int * int)) =
    if #1 dt < 1 then false
    else if (#2 dt < 1 orelse #2 dt > 12) then false
    else if #3 dt < 1 then false
    else if #3 dt > get_nth_int(days_in_months(#1 dt), #2 dt) then false
    else true








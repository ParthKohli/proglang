fun year (date : int*int*int) = (#1 date)
fun month (date : int*int*int) = (#2 date)
fun day (date : int*int*int) = (#3 date)

fun is_older (date_1 : int*int*int, date_2 : int*int*int) =
  (year date_1 < year date_2) 
  orelse (year date_1 = year date_2 andalso month date_1 < month date_2)
  orelse (year date_1 = year date_2 andalso month date_1 = month date_2 andalso
  day date_1 < day date_2) 

fun number_in_month (ds : (int*int*int) list, m : int) =
  if null ds then 0
  else
  (if month (hd ds) = m then 1 else 0) + number_in_month(tl ds, m)

fun number_in_months (ds : (int*int*int) list, ms : int list) =
  if null ms then 0
  else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)

fun dates_in_month (ds : (int*int*int) list, m : int) =
  if null ds then []
  else 
  let
    val later_dates = dates_in_month(tl ds, m)
  in 
    if (month (hd ds) = m)
    then (hd ds)::later_dates
    else later_dates 
  end

fun dates_in_months (ds : (int*int*int) list, ms : int list) =
  if null ms then []
  else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

fun get_nth (xs : string list, n : int) =
  if n = 1 
  then hd xs
  else get_nth(tl xs, n - 1)

fun date_to_string (date : int*int*int) =
  let val month_strings = ["January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"]
  in 
    get_nth(month_strings, month date) ^ " " ^ Int.toString(day date) ^ ", " ^
    Int.toString(year date) 
  end

fun number_before_reaching_sum (sum : int, xs : int list) =
  if sum <= hd xs then 0
  else 
    1 + number_before_reaching_sum(sum - (hd xs), tl xs)

fun what_month (d : int) =
  let 
    val monthwise_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in 1 + number_before_reaching_sum(d, monthwise_days)
  end

fun month_range (day1 : int, day2 : int) =
  if day1 > day2 then []
  else (what_month day1) :: month_range(day1 + 1, day2)

fun oldest (ds : (int*int*int) list) = 
  if null ds then NONE
  else if null (tl ds) then SOME(hd ds) 
  else
    let 
      val later_ans = valOf (oldest (tl ds))
    in 
      if is_older(hd ds, later_ans) then SOME(hd ds) else SOME(later_ans)
  end


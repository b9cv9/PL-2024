(****************************************************************************** 
  Шаблон для выполнения заданий лабораторной работы №1

  НЕ СЛЕДУЕТ УДАЛЯТЬ ИЛИ ПЕРЕСТАВЛЯТЬ МЕСТАМИ ЭЛЕМЕНТЫ, 
  ПРЕДСТАВЛЕННЫЕ В ШАБЛОНЕ (ВКЛЮЧАЯ КОММЕНТАРИИ). 
  ЭЛЕМЕНТЫ РЕШЕНИЯ СЛЕДУЕТ ВПИСЫВАТЬ В ПРОМЕЖУТКИ,
  ОПРЕДЕЛЕННЫЕ КОММЕНТАРИЯМИ.
 ******************************************************************************)

(****************************************************************************** 
  Загрузка определений модулей MyDate и Fixed и вспомогательных списков данных 
 ******************************************************************************)
use "lab-1-use.sml";

(****************************************************************************** 
  Задание 1 isLeapYear
 ******************************************************************************)
fun isLeapYear (year : int, isJulian : bool) : bool =
  if isJulian then year mod 4 = 0
  else year mod 400 = 0
  orelse year mod 100 <> 0 andalso year mod 4 = 0

(******************************************************************************)

(****************************************************************************** 
  Задание 2 isLongMonth
 ******************************************************************************)
fun isLongMonth (month : int) : bool =
  if month < 8 then (month + 1) mod 2 = 0
  else month mod 2 = 0

(******************************************************************************)

(****************************************************************************** 
  Задание 3 daysInMonth
 ******************************************************************************)
fun daysInMonth (d : date, isJulian : bool) : int =
  if (#2 d = 2 andalso isLeapYear (#3 d, isJulian)) 
  then 29
  else if #2 d = 2 
       then 28
       else if isLongMonth (#2 d)
            then 31
            else 30

(******************************************************************************)

(****************************************************************************** 
  Задание 4 isDayOK
 ******************************************************************************)
fun isDayOK (d : date, isJulian : bool) : bool =
  not (#1 d < 1 orelse #1 d > daysInMonth (d, isJulian))

(******************************************************************************)

(****************************************************************************** 
  Задание 5 isMonthOK
 ******************************************************************************)
fun isMonthOK (d : date) : bool =
  not (#2 d < 1 orelse #2 d > 12)

(******************************************************************************)

(****************************************************************************** 
  Задание 6 isCorrectDate
 ******************************************************************************)
fun isCorrectDate (d : date, isJulian : bool) : bool =
  isDayOK (d, isJulian) andalso isMonthOK d andalso #3 d > 0
(******************************************************************************)

(****************************************************************************** 
  Задание 7 incDateByNum
 ******************************************************************************)
fun incDateByNum (d : date, days : int, isJulian : bool) : date =
  let
    val day = #1 d
    val dayPlusDays = day + days
    val daysInM = daysInMonth (d, isJulian)
  in
    if dayPlusDays <= daysInM
    then MyDate.anotherDay (d, dayPlusDays) 
    else 
      let
        val month = #2 d
        val year = #3 d
        val newDays = days - daysInM + day - 1
      in
        if month < 12 then
          incDateByNum ((1, month + 1, year), newDays, isJulian)
        else incDateByNum ((1, 1, year + 1), newDays, isJulian)
      end
  end


(******************************************************************************)

(****************************************************************************** 
  Задание 8 decDateByNum
 ******************************************************************************)
fun decDateByNum (d : date, daysMinus : int, isJulian : bool) : date =
  let
    val day = #1 d
    val daysDiff = day - daysMinus
  in
    if daysDiff > 0 then 
      MyDate.anotherDay (d, daysDiff) 
    else
      let
        val year = #3 d
        val daysMinusDay = daysMinus - day
        val monthMinus1 = #2 d - 1
      in
        if monthMinus1 + 1 > 1 then 
          decDateByNum ( (daysInMonth ((day, monthMinus1, year), isJulian)
                         , monthMinus1, year
                         )
                       , daysMinusDay, isJulian
                       )
        else decDateByNum ((31, 12, year - 1), daysMinusDay, isJulian)
      end
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 9 newStyleCorrection
 ******************************************************************************)
fun newStyleCorrection (date : date) : int =
  let
    val year = #3 date - 1
    val century = year div 100
    val daysDiff = century - century div 4 - 2
  in
    if #2 date > 2 andalso isLeapYear (year + 1, true) 
                   andalso not (isLeapYear (year + 1, false))
    then daysDiff + 1
    else daysDiff
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 10 toJulianDay
 ******************************************************************************)
fun toJulianDay (grigDate : date) : date =
  decDateByNum (grigDate, newStyleCorrection grigDate, true)

(******************************************************************************)

(****************************************************************************** 
  Задание 11 toGrigorianDay
 ******************************************************************************)
fun toGrigorianDay (julianDate : date) : date =
  incDateByNum (julianDate, newStyleCorrection julianDate, true)

(******************************************************************************)

(****************************************************************************** 
  Задание 12 younger
 ******************************************************************************)
fun younger (date1 : date, date2 : date) : bool =
  let
    val y1 = #3 date1
    val y2 = #3 date2
    val m1 = #2 date1
    val m2 = #2 date2
  in
    if y1 <> y2      then y1 > y2
    else if m1 <> m2 then m1 > m2
    else #1 date1 > #1 date2
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 13 youngest
 ******************************************************************************)
fun youngest (l : (string * date) list) : (string * date) option =
  if null l then NONE
  else 
    let
      val head = hd l
      val tail = tl l
    in
      if null tail then SOME head
      else
        let
          val SOME res = youngest tail
        in
          if younger (#2 head, #2 res)
          then SOME head
          else SOME res
        end
    end

(******************************************************************************)

(****************************************************************************** 
  Задание 14 getNthFixed
 ******************************************************************************)
fun getNthFixed (n : int, l : fixed list) : fixed =
  if n = 0 then hd l 
  else getNthFixed (n - 1, tl l)
(******************************************************************************)

(****************************************************************************** 
  Задание 15 numToDigits
 ******************************************************************************)
fun numToDigits (num : int, numDigits : int) : int list =
  if numDigits = 0 then []
  else (if num = 0 then 0 else num mod 10)
       :: numToDigits (num div 10, numDigits - 1)

(******************************************************************************)

(****************************************************************************** 
  Задание 16 listElements
 ******************************************************************************)
fun listElements (firstL : int list, secL : fixed list list) : fixed list =
  if firstL = [] then []
  else getNthFixed (hd (firstL), hd (secL)) 
       :: listElements (tl (firstL), tl (secL))
(******************************************************************************)

(****************************************************************************** 
  Задание 17 listSum
 ******************************************************************************)
fun listSum (fl : fixed list) : fixed =
  if null fl then 0
  else hd fl + listSum (tl fl)

(******************************************************************************)

(****************************************************************************** 
  Задание 18 maxSmaller
 ******************************************************************************)
fun maxSmaller (l : fixed list, amount : fixed) : fixed = 
  if null l then 0
  else 
    let
      val maxTail = maxSmaller (tl l, amount)
      val head = hd l
    in
      if head < amount then
        if head > maxTail then head else maxTail
      else
        maxTail
    end

(******************************************************************************)

(****************************************************************************** 
  Задание 19 dateToCorrectionNums
 ******************************************************************************)
fun dateToCorrectionNums (d : date) : int list =
  #2 d - 1 :: numToDigits (#3 d, 4) @ [#3 d mod 4]

(******************************************************************************)

(****************************************************************************** 
  Задание 20 firstNewMoon
 ******************************************************************************)
fun firstNewMoon (actDate : date) : (fixed * date) option =
  let
    val m = #2 actDate
    val y = #3 actDate
    fun checkMonth (m : int, y : int) = if m < 3 then y - 1 else y
    val correctionNums = 
      dateToCorrectionNums (#1 actDate, m, checkMonth (m, y))
    val summ = listSum (listElements (correctionNums, corrections))
    val diff = Fixed.fromInt (newStyleCorrection actDate)
    val totalSum = diff + summ
    val reduction = maxSmaller (reductions, totalSum - Fixed.fromInt 1)
    val fnum = if reduction = 0 then totalSum else totalSum - reduction
    val newDate = (Fixed.toInt fnum, m, y)
  in
    if isCorrectDate (newDate, false)
    then SOME (fnum, newDate)
    else NONE
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 21 winterSolstice
 ******************************************************************************)
fun winterSolstice (year : int) : date =
  let
    val totalDays = 2250000 + 24220 * year - 100000 
                    * (year div 4 - year div 100 + year div 400)
  in
    (Fixed.toInt totalDays, 12, year)
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 22 chineseNewYearDate
 ******************************************************************************)
fun chineseNewYearDate (y : int) : date =
  let
    val yMinus1 = y - 1
    val dayFixed = #1 (valOf (firstNewMoon (1, 12, yMinus1)))
  in
    if younger ( (Fixed.toInt dayFixed, 12, yMinus1)
               , winterSolstice yMinus1
               ) 
    then incDateByNum ( (Fixed.toInt dayFixed, 12, yMinus1)
                      , Fixed.toInt 2953059, false
                      )
    else incDateByNum ( (Fixed.toInt dayFixed, 12, yMinus1)
                      , Fixed.toInt 5906118, false
                      )
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 23 getNthString
 ******************************************************************************)
fun getNthString (n : int, l : string list) : string =
  if n = 0 then hd l 
  else getNthString (n - 1, tl l)

(******************************************************************************)

(****************************************************************************** 
  Задание 24 dateToString
 ******************************************************************************)
fun dateToString (d : date) : string =
  getNthString (#2 d - 1, months) ^ " " 
                                  ^ Int.toString(#1 d) 
                                  ^ ", " 
                                  ^ Int.toString(#3 d)

(******************************************************************************)

(****************************************************************************** 
  Задание 25 chineseYear
 ******************************************************************************)
fun chineseYear (y : int) : string * string * string * string =
  let
    val cycleYear = (y + 2396) mod 60
    val celestialIndex = cycleYear mod 10
    val terrestrialIndex = cycleYear mod 12

    val celestialName = getNthString (celestialIndex, celestialChi)
    val colorName = getNthString (celestialIndex div 2, celestialColor)
    val terrestrialName = getNthString (terrestrialIndex, terrestrialChi)
    val animalName = getNthString (terrestrialIndex, terrestrialEng)
    val celestialEngName = getNthString (celestialIndex, celestialEng)
  in
    ( celestialName ^ "-" ^ terrestrialName
    , colorName, animalName, celestialEngName
    )
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 26 dateToChineseYear
 ******************************************************************************)
fun dateToChineseYear (d : date) : string * string * string * string =
  let
    val month = #2 d
    val year = #3 d
    val chineNYD = chineseNewYearDate (#3 d)
    val chineNYDMonth = #2 chineNYD
  in
    if month < chineNYDMonth orelse month = chineNYDMonth
                             andalso #1 d < #1 chineNYD
    then chineseYear (year - 1)
    else chineseYear year
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 27 dateToAnimal
 ******************************************************************************)
fun dateToAnimal (d : date) : string =
  #3 (dateToChineseYear d)

(******************************************************************************)

(****************************************************************************** 
  Задание 28 animal
 ******************************************************************************)
fun animal (l : string * date) : string =
  dateToAnimal (#2 l)

(******************************************************************************)

(****************************************************************************** 
  Задание 29 extractAnimal
 ******************************************************************************)
fun extractAnimal (l : (string * date) list, animalName : string)
                  : (string * date) list = 
  if null l then []
  else
    let
      val head = hd l
      val birthDate = #2 head
      val tail = tl l
    in
      if dateToAnimal birthDate = animalName then
        (#1 head, birthDate) :: extractAnimal (tail, animalName)
      else
        extractAnimal (tail, animalName)
    end

(******************************************************************************)

(****************************************************************************** 
  Задание 30 extractAnimals
 ******************************************************************************)
fun extractAnimals (l1 : (string * date) list, l2 : string list) 
                   : (string * date) list =
  if null l1 orelse null l2 then []
  else
    extractAnimal (l1, hd l2) @ extractAnimals (l1, tl l2)

(******************************************************************************)

(****************************************************************************** 
  Задание 31 youngestFromAnimals
 ******************************************************************************)
fun youngestFromAnimals (l1 : (string * date) list, l2 : string list)
                        : (string * date) option =
  let
    val studentsList = extractAnimals (l1, l2)
  in
    if null studentsList then NONE
    else youngest studentsList
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 32 oldStyleStudents
 ******************************************************************************)
fun oldStyleStudents (l : (string * date) list) 
                     : (string * date) list =
  if null l then [] 
  else
    let
      val head = hd l
    in
      (#1 head, toGrigorianDay (#2 head)) 
      :: oldStyleStudents (tl l)
    end

(******************************************************************************)

(****************************************************************************** 
  Задание 33 youngestFromOldStyleAnimals
 ******************************************************************************)
fun youngestFromOldStyleAnimals (l1 : (string * date) list, l2 : string list)
                                : (string * date) option =
  let
    val grigDateAnsCheck = 
       youngestFromAnimals (oldStyleStudents l1, l2)
  in
    if isSome grigDateAnsCheck then 
      let
        val grigDateAns = valOf grigDateAnsCheck
      in
        SOME (#1 grigDateAns, toJulianDay (#2 grigDateAns))
      end
    else NONE
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 34 listOfStringDates
 ******************************************************************************)
fun listOfStringDates (l : (string * date) list) 
                      : (string * string) list =
  if null l then []
  else
    let
      val head = hd l
    in
      (#1 head, dateToString (#2 head)) :: listOfStringDates (tl l)
    end

(******************************************************************************)

(****************************************************************************** 
  Задание 35 oldStyleStudentStringDates
 ******************************************************************************)
fun oldStyleStudentStringDates (l : (string * date) list)
                               : (string * string) list =
  listOfStringDates (oldStyleStudents l)

(******************************************************************************)
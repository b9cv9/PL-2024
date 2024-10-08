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
fun isLeapYear (year : int, isJulianCalendar : bool) : bool =
  if isJulianCalendar then year mod 4 = 0
  else year mod 400 = 0 orelse year mod 100 <> 0 andalso year mod 4 = 0

(******************************************************************************)

(****************************************************************************** 
  Задание 2 isLongMonth
 ******************************************************************************)
fun isLongMonth (month : int) : bool =
  month = 1 orelse month = 3 orelse month = 5 orelse month = 7
  orelse month = 8 orelse month = 10 orelse month = 12

(******************************************************************************)

(****************************************************************************** 
  Задание 3 daysInMonth
 ******************************************************************************)
fun daysInMonth (inputDate : date, isJulianCalendar : bool) : int =
  let
    val month = #2 inputDate
  in
    if month = 2 then
      if isLeapYear (#3 inputDate, isJulianCalendar) then 29 else 28
    else if isLongMonth month then 31
    else 30
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 4 isDayOK
 ******************************************************************************)
fun isDayOK (inputDate : date, isJulianCalendar : bool) : bool =
  let
    val day = #1 inputDate
  in
    day >= 1 andalso day <= daysInMonth (inputDate, isJulianCalendar)
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 5 isMonthOK
 ******************************************************************************)
fun isMonthOK (inputDate : date) : bool =
  let
    val month = #2 inputDate
  in
    month >= 1 andalso month <= 12
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 6 isCorrectDate
 ******************************************************************************)
fun isCorrectDate (inputDate : date, isJulianCalendar : bool) : bool =
  isDayOK (inputDate, isJulianCalendar)
  andalso isMonthOK inputDate
  andalso #3 inputDate > 0

(******************************************************************************)

(****************************************************************************** 
  Задание 7 incDateByNum
 ******************************************************************************)
fun incDateByNum ( inputDate : date
                 , numDays : int
                 , isJulianCalendar : bool
                 )
                 : date = 
  let
    fun addDays (d : int, m : int, y : int, daysToAdd : int) : date =
      let
        val daysInCurMonth = daysInMonth ((d, m, y), isJulianCalendar)
        val dPlusDaysToAdd = d + daysToAdd
      in
        if dPlusDaysToAdd <= daysInCurMonth then (dPlusDaysToAdd, m, y)
        else
          addDays ( 1
                  , if m < 12 then m + 1 else 1
                  , if m < 12 then y else y + 1
                  , dPlusDaysToAdd - daysInCurMonth - 1
                  )
      end
  in
    addDays (#1 inputDate, #2 inputDate, #3 inputDate, numDays)
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 8 decDateByNum
 ******************************************************************************)
fun decDateByNum ( inputDate : date
                 , numDays : int
                 , isJulianCalendar : bool
                 )
                 : date = 
  let
    val day = #1 inputDate
    val month = #2 inputDate
    val year = #3 inputDate
    fun subDays (d : int, m : int, y : int, daysToSub : int) : date =
      let
        val dMinusDaysToSub = d - daysToSub
      in
        if dMinusDaysToSub >= 1 then (dMinusDaysToSub, m, y)
        else
          let 
            val newMonth = if m = 1 then 12 else m - 1
            val newYear = if m = 1 then y - 1 else y
          in
            subDays ( daysInMonth ((1, newMonth, newYear), isJulianCalendar)
                    , newMonth
                    , newYear
                    , ~dMinusDaysToSub
                    )
          end
      end
  in
    subDays (day, month, year, numDays)
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 9 newStyleCorrection
 ******************************************************************************)
fun newStyleCorrection (inputDate : date) : int =
  let
    val inputMonth = #2 inputDate
    val inputYear = #3 inputDate
    val dif = inputYear div 100 - inputYear div 400 - 2
    val pass = inputMonth > 2 orelse inputMonth = 2 andalso #1 inputDate = 29
    val julianLeapDif =
      if isLeapYear (inputYear, true) then
        if pass then dif
        else dif - 1
      else dif
  in
    if isLeapYear (inputYear, false) then
      if pass then julianLeapDif
      else julianLeapDif + 1
    else julianLeapDif
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 10 toJulianDay
 ******************************************************************************)
fun toJulianDay (inputDate : date) : date =
  decDateByNum (inputDate, newStyleCorrection (inputDate), true)

(******************************************************************************)

(****************************************************************************** 
  Задание 11 toGrigorianDay
 ******************************************************************************)
fun toGrigorianDay (inputDate : date) : date =
  if #1 inputDate = 29 andalso #2 inputDate = 2 then
    let
      val newMarchDate = (1, 3, #3 inputDate)
    in
      decDateByNum ( incDateByNum ( newMarchDate
                                  , newStyleCorrection newMarchDate
                                  , false
                                  )
                   , 1
                   , false
                   )
    end
  else
    incDateByNum (inputDate, newStyleCorrection inputDate, false)

(******************************************************************************)

(****************************************************************************** 
  Задание 12 younger
 ******************************************************************************)
fun younger ((d1, m1, y1) : date, (d2, m2, y2) : date) : bool =
  if y1 <> y2 then y1 > y2
  else if m1 <> m2 then m1 > m2
  else d1 > d2

(******************************************************************************)

(****************************************************************************** 
  Задание 13 youngest
 ******************************************************************************)
fun youngest (l : (string * date) list) : (string * date) option =
  if null l then NONE
  else if null (tl l) then SOME (hd l)
  else
    let
      val headL = hd l
      val date1 = #2 headL
      val tailL = tl l
      val headOfTailL = hd tailL
      val date2 = #2 headOfTailL
    in
      if younger (date1, date2) then
        youngest ((#1 headL, date1) :: tl tailL)
      else
        youngest ((#1 headOfTailL, date2) :: tl tailL)
    end

(******************************************************************************)

(****************************************************************************** 
  Задание 14 getNthFixed
 ******************************************************************************)
fun getNthFixed (n : int, xs : fixed list) : fixed =
  if n = 0 then hd xs
  else getNthFixed (n - 1, tl xs)

(******************************************************************************)

(****************************************************************************** 
  Задание 15 numToDigits
 ******************************************************************************)
fun numToDigits (num : int, numDigits : int) : int list =
  if numDigits = 0 then []
  else num mod 10 :: numToDigits (num div 10, numDigits - 1)

(******************************************************************************)

(****************************************************************************** 
  Задание 16 listElements
 ******************************************************************************)
fun listElements (indexes : int list, lists : fixed list list) : fixed list =
  if null indexes orelse null lists then []
  else getNthFixed (hd indexes, hd lists) :: listElements (tl indexes, tl lists)

(******************************************************************************)

(****************************************************************************** 
  Задание 17 listSum
 ******************************************************************************)
fun listSum (xs : fixed list) : fixed =
  if null xs then 0
  else hd xs + listSum (tl xs)

(******************************************************************************)

(****************************************************************************** 
  Задание 18 maxSmaller
 ******************************************************************************)
fun maxSmaller (xs : fixed list, amount : fixed) : fixed =
  if null xs then 0
  else
    let
      val x = hd xs
    in
      if x < amount then
        let 
          val maxFromRest = maxSmaller (tl xs, amount)
        in
          if x > maxFromRest then x else maxFromRest
        end
      else 
        maxSmaller (tl xs, amount)
    end

(******************************************************************************)

(****************************************************************************** 
  Задание 19 dateToCorrectionNums
 ******************************************************************************)
fun dateToCorrectionNums (d : date) : int list =
  let
    val year = #3 d
  in
    (#2 d - 1) :: numToDigits (year, 4) @ [year mod 4]
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 20 firstNewMoon
 ******************************************************************************)
fun firstNewMoon (inputDate : date) : (fixed * date) option =
  let
    val month = #2 inputDate
    val year = #3 inputDate
    (* Суммируем значения корректировок + разница в днях (между календарями) *)
    val bigSum = 
      listSum (listElements ( dateToCorrectionNums ( #1 inputDate
                                                   , month
                                                   , if month <= 2 then year - 1
                                                     else year 
                                                   )
                            , corrections ))
      + Fixed.fromInt (newStyleCorrection inputDate)
    (* Вычитаем найденное значение (возможно равное 0) из общей суммы *)
    val fnum = bigSum - maxSmaller (reductions, bigSum - Fixed.fromInt 1)
    (* Преобразуем fnum в целое число и формируем дату *)
    val newDate = MyDate.anotherDay (inputDate, Fixed.toInt fnum)
  in
    if isCorrectDate (newDate, false) then SOME (fnum, newDate)
    else NONE
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 21 winterSolstice
 ******************************************************************************)
fun winterSolstice (y : int) : date =
  ( Fixed.toInt ( 2250000 
                  + 24220 * y
                  - Fixed.fromInt (y div 4 - y div 100 + y div 400) )
  , 12
  , y
  )

(******************************************************************************)

(****************************************************************************** 
  Задание 22 chineseNewYearDate
 ******************************************************************************)
fun chineseNewYearDate (numberOfYear : int) : date =
  let
    val prevNumberOfYear = numberOfYear - 1
    (* Получаем дату зимнего солнцестояния для предыдущего года *)
    val winterSolsticeDate = winterSolstice prevNumberOfYear
    (* Получаем первое новолуние для декабря предыдущего года *)
    val resultFirstNewMoon = valOf (firstNewMoon (1, 12, prevNumberOfYear))
    val firstMoonDate = #2 resultFirstNewMoon
    val newDay = 
      if younger (winterSolsticeDate, firstMoonDate)
         orelse winterSolsticeDate = firstMoonDate
      then
        #1 resultFirstNewMoon + 5906118
      else 
        #1 resultFirstNewMoon + 2953059
    val probablyFebruaryDay = Fixed.toInt (newDay - 6200000)
  in
    if probablyFebruaryDay > 0 then
      (probablyFebruaryDay, 2, numberOfYear)
    else
      (Fixed.toInt (newDay - 3100000), 1, numberOfYear)
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 23 getNthString
 ******************************************************************************)
fun getNthString (n : int, strList : string list) : string =
  if n = 0 then hd strList
  else getNthString (n - 1, tl strList)

(******************************************************************************)

(****************************************************************************** 
  Задание 24 dateToString
 ******************************************************************************)
fun dateToString (d : date) : string =
  getNthString (#2 d - 1, months)
  ^ " "
  ^ Int.toString (#1 d)
  ^ ", "
  ^ Int.toString (#3 d)

(******************************************************************************)

(****************************************************************************** 
  Задание 25 chineseYear
 ******************************************************************************)
fun chineseYear (numberOfGrigorianYear : int) 
                : string * string * string * string =
  let
    val chineseYearNumber = (numberOfGrigorianYear + 2396) mod 60
    val celestialNumber = chineseYearNumber mod 10
    val terrestrialNumber = chineseYearNumber mod 12
  in
    ( getNthString (celestialNumber, celestialChi)
      ^ "-" 
      ^ getNthString (terrestrialNumber, terrestrialChi)
    , getNthString (celestialNumber div 2, celestialColor)
    , getNthString (terrestrialNumber, terrestrialEng)
    , getNthString (celestialNumber, celestialEng)
    )
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 26 dateToChineseYear
 ******************************************************************************)
fun dateToChineseYear (d : date) : string * string * string * string =
  let
    val year = #3 d
  in
    (* Если дата до китайского Нового года, то используем предыдущий год *)
    if younger (chineseNewYearDate year, d) then
      chineseYear (year -1)
    else
      chineseYear year
  end

(******************************************************************************)

(****************************************************************************** 
  Задание 27 dateToAnimal
 ******************************************************************************)
fun dateToAnimal (d : date) : string = #3 (dateToChineseYear d)

(******************************************************************************)

(****************************************************************************** 
  Задание 28 animal
 ******************************************************************************)
fun animal (nameStudent : string, d : date) : string = dateToAnimal d

(******************************************************************************)

(****************************************************************************** 
  Задание 29 extractAnimal
 ******************************************************************************)
fun extractAnimal ( students : (string * date) list
                  , inputAnimal : string 
                  ) 
                  : (string * date) list =
  if null students then []
  else
    let
      val headStudents = hd students
    in
      if animal (#1 headStudents, #2 headStudents) = inputAnimal then
        headStudents :: extractAnimal (tl students, inputAnimal)
      else
        extractAnimal (tl students, inputAnimal)
    end

(******************************************************************************)

(****************************************************************************** 
  Задание 30 extractAnimals
 ******************************************************************************)
fun extractAnimals ( students : (string * date) list
                   , animals : string list 
                   ) 
                   : (string * date) list =
  if null animals then []
  else if null students then []
  else
    let
      val headStudents = hd students
      fun animalForStudent (animals : string list) : bool =
        if null animals then false
        else
          animal (#1 headStudents, #2 headStudents) = hd animals
          orelse animalForStudent (tl animals)
    in
      if animalForStudent animals then 
        headStudents :: extractAnimals (tl students, animals)
      else
        extractAnimals (tl students, animals)
    end

(******************************************************************************)

(****************************************************************************** 
  Задание 31 youngestFromAnimals
 ******************************************************************************)
fun youngestFromAnimals ( students : (string * date) list
                        , animals : string list 
                        ) 
                        : (string * date) option =
  youngest (extractAnimals (students, animals))

(******************************************************************************)

(****************************************************************************** 
  Задание 32 oldStyleStudents
 ******************************************************************************)
fun oldStyleStudents (students : (string * date) list) : (string * date) list = 
  if null students then []
  else 
    let
      val headStudents = hd students
    in
      (#1 headStudents, toGrigorianDay (#2 headStudents))
      :: oldStyleStudents (tl students)
    end

(******************************************************************************)

(****************************************************************************** 
  Задание 33 youngestFromOldStyleAnimals
 ******************************************************************************)
fun youngestFromOldStyleAnimals ( students : (string * date) list
                                , animals : string list 
                                ) 
                                : (string * date) option =
  let 
    val youngestStudentGrigorian =
      youngestFromAnimals (oldStyleStudents students, animals)
  in
    if isSome youngestStudentGrigorian then
      let
        val youngestStudentGrigorianValue = valOf youngestStudentGrigorian
      in
        SOME ( #1 youngestStudentGrigorianValue
             , toJulianDay (#2 youngestStudentGrigorianValue)
             )
      end
    else NONE
  end

(******************************************************************************)


(******************************************************************************)

(****************************************************************************** 
  Задание 34 listOfStringDates
 ******************************************************************************)
fun listOfStringDates (students : (string * date) list)
                      : (string * string) list =
  if null students then []
  else
    let
      val headStundets = hd students
    in
      (#1 headStundets, dateToString (#2 headStundets))
      :: listOfStringDates (tl students)
    end

(******************************************************************************)

(****************************************************************************** 
  Задание 35 oldStyleStudentStringDates
 ******************************************************************************)
fun oldStyleStudentStringDates (students : (string * date) list)
                               : (string * string) list =
  if null students then []
  else
    let
      val headStundets = hd students
    in
      (#1 headStundets, dateToString (toGrigorianDay (#2 headStundets)))
      :: oldStyleStudentStringDates (tl students)
    end

(******************************************************************************)

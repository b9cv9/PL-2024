(******************************************************************************
  Модуль для работы с датами 
 ******************************************************************************)
structure MyDate = struct
  type date = int * int * int

  fun anotherDay (d : date, newDay : int) : date =
    (newDay, #2 d, #3 d)

  fun anotherMonth (d : date, newMonth : int) : date =
    (#1 d, newMonth, #3 d)

  fun anotherYear (d : date, newYear : int) : date =
    (#1 d, #2 d, newYear)
end

(******************************************************************************
  Модуль для работы с числами с фиксированной точкой
 ******************************************************************************)
structure Fixed = struct
  type fixed = int
  fun fromInt (n : int) : fixed = n * 100000
  fun toInt (f : fixed) : int = f div 100000
end

(******************************************************************************
  Синонимы типов для даты и для числа с фиксированной точкой
 ******************************************************************************)
type fixed = Fixed.fixed
type date = MyDate.date

(******************************************************************************
  Списки значений для вычислений 
 ******************************************************************************)
(* Корректировки для вычисления даты новолуния *)
val corrections = (* monthCorrection  years< calendar*)
  [ [ 1340000, 1190000, 2420000, 2260000, 2200000, 2060000, 2000000, 1840000
    , 1700000, 1660000, 1510000, 1480000 ]
  , [ 0, 1860000, 780000, 2640000, 1550000, 460000, 2330000, 1240000, 150000
    , 2020000 ]
  , [ 0, 930000, 1860000, 2790000, 760000, 1690000, 2620000, 600000, 1530000
    , 2460000 ]
  , [ 0, 430000, 870000, 1300000, 1740000, 2170000, 2600000, 80000, 520000
    , 950000 ]
  , [ 0, 1390000, 2770000, 1210000, 2590000, 1030000, 2420000 ]
  , [0, 20000, 50000, 80000]
  ]

(* Числа предельной разности для вычисления даты новолуния *)
val reductions = [8859177, 17718354, 5906118, 2953059, 14765295, 11812236] 

(* Месяцы года *)
val months = [ "January", "February", "March", "April", "May", "June"
             , "July", "August", "September", "October", "November"
             , "December" ]

(* Наименования небесных стихий на китайском языке *)
val celestialChi   = [ "Jia", "Yi", "Bing", "Ding", "Wu", "Ji", "Geng", "Xin"
                     , "Ren", "Gui" ]

(* Наименования небесных стихий на английском языке языке *)
val celestialEng   = [ "Growing wood", "Cut timber", "Natural fire"
                     , "Artificial fire", "Earth", "Earthenware"
                     , "Metal", "Wrought metal", "Running water"
                     , "Standing water" ]

(* Наименования цвета небесных стихий на английском языке *)
val celestialColor = ["Green", "Red", "Brown", "White", "Black"] 

(* Наименования земных стихий на китайском языке *)
val terrestrialChi = [ "Zi", "Chou", "Yin", "Mao", "Chen", "Si", "Wu", "Wei"
                      , "Shen", "You", "Xu", "Hai" ]

(* Наименования земных стихий на английском языке *)
val terrestrialEng = [ "Rat", "Cow", "Tiger", "Rabbit", "Dragon", "Snake"
                     , "Horse", "Sheep", "Monkey", "Chicken", "Dog", "Pig" ]

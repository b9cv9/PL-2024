fun f2 (v1 : int, v2 : int, v3 : int, v4 : int) : int =
  let
    fun chet (x : int) : int =
      if x mod 2 = 0 then 1 else 0
    fun krat (x : int) : int =
      if x mod 3 = 0 then 1 else 0
    val a = chet v1 + chet v2 + chet v3 + chet v4
    val b = krat v1 + krat v2 + krat v3 + krat v4
  in
    if b = 0      then 1
    else if b = 1 then a
    else if b = 2 then a * a
    else if b = 3 then a * a * a
    else a * a * a * a
  end

(* ТЕСТОВЫЕ ЗАПУСКИ *)
val test1 = f2 (9, 2, 3, 4)
val test2 = f2 (6, 15, 20, 3)
val test3 = f2 (1, 2, 3, 4)
val test4 = f2 (~3, ~6, ~9, ~12)
val test5 = f2 (3, 6, 9, 12)
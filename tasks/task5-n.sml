fun f5 (x : real, n : int) : real =
  let 
    (* ¬спомогательна€ функци€, реализующа€ цикл *)
    fun f5Iter ( i : int         (* счетчик слагаемых *)
               , accum : real    (* аккумул€тор суммы *)
               , fourDeg : real  (* степень четверки *)
               , threeDeg : real (* степень тройки *)
               , sign : real     (* знак степени тройки в числителе *)
               , xDeg : real )   (* степень икса *)
               :real =
      if i > n then accum
      else 
        f5Iter ( i + 1
               , accum 
                 + (fourDeg - sign * threeDeg) / (fourDeg * threeDeg) * xDeg
               , fourDeg * 4.0
               , threeDeg * 3.0
               , sign * ~1.0
               , xDeg * x )
  in
    (* передаем во вспомогательную функцию параметры первого слагаемого *)
    f5Iter (0, 0.0, 4.0, 3.0, ~1.0, 1.0)
  end

(* функци€ дл€ проверки результата разложени€ в р€д *)
fun f5Test (x : real) : real = 7.0 / (12.0 - x - x * x)

val test11 = f5 (0.3, 100)
val test12 = f5Test 0.3

val test21 = f5 (0.9, 100)
val test22 = f5Test 0.9

val test31 = f5 (1.8, 100)
val test32 = f5Test 1.8


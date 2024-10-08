fun f5 (x : real, n : int) : real =
  let
    val pow2 = x * x
    (* Вспомогательная функция, реализующая цикл *)
    fun f5Iter ( i : int (* счетчик слагаемых *)
               , accum : real (* аккумулятор суммы *)
               , num : real (* числитель *)
               , xPow : real
               , factorial : real
               , fact : real) (* степень x *)
               : real =
      if i > n then accum
      else
        f5Iter ( i + 1
               , accum
                 + num / factorial * xPow
               , num + 2.0
               , xPow * pow2 
               , factorial * fact * (fact + 1.0) / num
               , fact + 2.0 )
    in
      f5Iter (0, x, 1.0, x * pow2, 6.0, 4.0)
    end



(* функция для проверки результата разложения в ряд *)
fun f5Test (x : real) : real = Math.asin x

val test11 = f5 (0.3, 100)
val test12 = f5Test 0.3
val test21 = f5 (0.9, 100)
val test22 = f5Test 0.9
val test31 = f5 (0.5, 100)
val test32 = f5Test 0.5
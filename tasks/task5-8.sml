fun f5 (x : real, n : int) : real =
  let 
    val doubleX = x * x
    fun f5Iter ( i : int
               , accum : real
               , sign : real
               , twoDeg : real
               , xDeg : real
               , factorial : real
               , fact : real )
               : real =
      if i > n then accum
      else 
        f5Iter ( i + 1
               , accum 
                 + sign * twoDeg * xDeg / factorial
               , sign * ~1.0
               , twoDeg * 4.0
               , xDeg * doubleX
               , factorial * fact * (fact + 1.0)
               , fact + 2.0 )
  in
    f5Iter (1, 0.0, 1.0, 2.0, doubleX, 2.0, 3.0)
  end

(* функция для проверки результата разложения в ряд *)
fun f5Target (x : real) : real = Math.sin x * Math.sin x

val test11 = f5 (0.3, 100)
val test12 = f5Target 0.3

val test21 = f5 (0.9, 100)
val test22 = f5Target 0.9

val test31 = f5 (1.8, 100)
val test32 = f5Target 1.8

val test41 = f5 (3.0, 100)
val test42 = f5Target 3.0


val test51 = f5 (3.0, 1)
val test52 = f5Target 3.0
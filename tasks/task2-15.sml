fun f2 (v1 : int, v2 : int, v3 : int, v4 : int) : int =
  let
    fun parity (x : int) : int =
      if x mod 2 = 0 then 1 else 0
    fun multiplicity (x : int) : int =
      if x mod 3 = 0 then 1 else 0
    val a = parity v1 + parity v2
                      + parity v3
                      + parity v4
    val b = multiplicity v1 + multiplicity v2
                            + multiplicity v3
                            + multiplicity v4
  in
    if b = 0      then 1
    else if b = 1 then a
    else
      let
        val doubleA = a * a
      in
        if b = 2      then doubleA
        else if b = 3 then doubleA * a
        else doubleA * doubleA
      end
  end

(* ТЕСТОВЫЕ ЗАПУСКИ *)
val test1 = f2 (9, 2, 3, 4)
val test2 = f2 (6, 15, 20, 3)
val test3 = f2 (6, 12, 18, 24)
val test4 = f2 (~3, ~6, ~9, ~12)
val test5 = f2 (3, 6, 9, 12)
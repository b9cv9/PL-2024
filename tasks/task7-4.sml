fun f7 n = 
  if n < 0 then 
    0.0
  else
    let
      fun outer_sum (i, i_max, j_max, accum) =
        if i_max = 1 then 
          accum
        else
          let
            val realIMax = real i_max
            val inner_result = inner_sum ( i_max
                                         , j_max
                                         , 0.0
                                         , realIMax
                                         , Math.log10 realIMax
                                         )
          in
            outer_sum (i, i_max - 1, j_max, accum + inner_result)
          end
      and inner_sum (i, j_max, j_accum, fromIntI, logFromIntI) =
        if j_max = 0 then 
          j_accum
        else
          let
            val term = logFromIntI + fromIntI / real j_max
          in
            inner_sum ( i
                      , j_max - 1
                      , j_accum + term
                      , fromIntI
                      , logFromIntI
                      )
          end
    in
      outer_sum (2, n + 2, n + 1, 0.0)
    end


(* ТЕСТОВЫЕ ЗАПУСКИ *)
val test0 = f7 1
val test1 = f7 2
val test2 = f7 5
val test3 = f7 ~2
val test4 = f7 0
val test5 = f7 ~1
fun f7 n = 
  let
    fun outer_sum (i, i_max, j_max, accum) =
      if i_max = 1 then accum
      else
        let
          val inner_result = inner_sum (i_max, j_max, 0.0)
        in
          outer_sum (i, i_max - 1, j_max, accum + inner_result)
        end
    and inner_sum (i, j_max, j_accum) =
      if j_max = 0 then j_accum
      else
        let
          val fromIntI = real i
          val term = Math.log10 fromIntI 
                     + fromIntI / real j_max
        in
          inner_sum (i, j_max - 1, j_accum + term)
        end
  in
    outer_sum (2, n + 2, n + 1, 0.0)
  end

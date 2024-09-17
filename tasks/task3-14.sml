fun f3 (a1 : (int * real * real) list, a2 : real * int list)
       : (real * real) list =
  let
    val rxPlus4 = real (hd (tl (#2 a2))) + 4.0
    val yp7 = 7.0 + #3 (hd (tl (tl a1)))
    val rxPlus4DivideY = rxPlus4 / yp7
    val rxPlus5 = rxPlus4 + 1.0
  in
    [ (rxPlus5, rxPlus4DivideY)
    , (yp7, rxPlus5 / rxPlus4)
    , (rxPlus4, rxPlus4DivideY)
    ]
  end


val test0 = f3 ( [(1, 5.0, 1.3), (2, 6.4, 0.3)
               , (1, 5.0, 2.3), (1, 5.0, 3.3)]
               , (7.5, [1, 3, 2, 4])
               )
val test1 = f3 ( [(4, 6.1, 3.2), (5, 7.3, 1.8)
               , (1, 5.0, 4.3)], (8.3, [0, 7, 5, 1])
               )
val test2 = f3 ( [(9, 3.0, 4.1), (3, 4.7, 2.5)
               , (1, 5.0, 5.3), (1, 5.0, 2.3)
               , (1, 5.0, 2.3)], (6.9, [2, 5, 1, 6])
               )
val test3 = f3 ( [(8, 2.2, 5.5), (7, 3.9, 1.4)
               , (1, 5.0, 6.3)], (5.8, [4, 9, 0, 3])
               )
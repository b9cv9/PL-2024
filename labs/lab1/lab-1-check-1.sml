fun f6 ([], []) = []
  | f6 ([], _) = []
  | f6 (_, []) = []
  | f6 (x::xe, y::ye) = (x, y) :: f6 (xe, ye)

val test0 = f6 ([], [])
val test1 = f6 ([1.4, 1.5, 4.7], [9, 8, 9, 7])
val test2 = f6 ([1.4, 1.5, 4.7, 5.5, 3.4], [1, 2])
val test3 = f6 ([], [6])
val test4 = f6 ([7.7], [])
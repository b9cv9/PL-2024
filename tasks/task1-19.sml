fun f1 (x : real, y : real) : real =
  abs x * abs y

val test1 = f1 (3.5, 5.0) (* 17.5 *)
val test2 = f1 (5.0, 2.5) (* 12.5 *)
val test3 = f1 (1.0, 10.5) (* 10.5 *)
val test4 = f1 (~1.5, 10.0) (* 15.0 *)
val test5 = f1 (~5.5, ~2.0) (* 11.0 *)
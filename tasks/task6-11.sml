fun f6 (xs, k) =
  let
    fun helper ([], _) _ = []
      | helper (x :: xs, k) n =
          if n mod k = 0 then helper (xs, k) (n + 1)
          else x :: helper (xs, k) (n + 1)
  in
    helper (xs, k) 1
  end

(* ТЕСТОВЫЕ ЗАПУСКИ *)
val test0 = f6 ([3.0, 1.0, 3.0, 2.0, 3.0, 5.0, 4.0, 4.0, 7.0], 3);
val test1 = f6 ([1.2, 1.3, 1.4, 1.5, 1.6, 1.7], 2);
val test2 = f6 ([10.0, 20.0, 30.0, 40.0, 50.0, 60.0], 3);
val test3 = f6 ([5.5, 6.6, 7.7], 1);
val test4 = f6 ([8.1, 9.2], 4);
val test5 = f6 ([3.0, 1.0, 3.0, 2.0, 3.0, 5.0, 4.0, 4.0, 7.0], 3);
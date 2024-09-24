fun f4 (lst : real list, k : int) : real list = 
  let
    fun removeK (lst : real list, index : int) 
                : real list =
      if null lst then []
      else 
        let
          val removeKValue = removeK (tl lst, index + 1)
        in
          if index mod k = 0 then
            removeKValue
          else
            hd lst :: removeKValue
        end
  in
    if k <= 0 then lst 
    else 
      removeK (lst, 1)
  end

(* ТЕСТОВЫЕ ЗАПУСКИ *)
val test0 = f4 ([3.0, 1.0, 3.0, 2.0, 3.0, 5.0, 4.0, 4.0, 7.0], 3);
val test1 = f4 ([1.2, 1.3, 1.4, 1.5, 1.6, 1.7], 2);
val test2 = f4 ([10.0, 20.0, 30.0, 40.0, 50.0, 60.0], 3);
val test3 = f4 ([5.5, 6.6, 7.7], 1);
val test4 = f4 ([8.1, 9.2], 4);
val test5 = f4 ([3.0, 1.0, 3.0, 2.0, 3.0, 5.0, 4.0, 4.0, 7.0], 3);

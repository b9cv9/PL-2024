-- Рекуррентная последовательность, начиная с y0 = 1 и y1 = x / 2
f11 x n = take (n + 1) lst
  where
    -- список начинается с двух начальных значений
    lst = 1 : x / 2 : elemLst lst 2
    -- функция вычисляет следующий элемент последовательности по рекуррентной формуле
    elemLst (y0 : ys@(y1 : _)) i = 
      if i > n then [] -- завершение по достижению n элементов
      else 2 * y1 - (x^2 / fromIntegral(i^2)) * y0 : elemLst ys (i + 1)

-- Тестовые запуски
main = do
  let res1 = f11 0.5 10
  putStrLn "f11 0.5 10"
  print res1
  
  let res2 = f11 1.0 15
  putStrLn "f11 1.0 15"
  print res2
  
  let res3 = f11 2.0 20
  putStrLn "f11 2.0 20"
  print res3

(****************************************************************************** 
  Шаблон для выполнения заданий лабораторной работы №1
 ******************************************************************************)

(****************************************************************************** 
  Загрузка файла с лабораторной работой 
 ******************************************************************************)
use "lab-2.sml";

(****************************************************************************** 
  ТЕСТЫ К РЕШЕНИЯМ
  Здесь приведены по большей части тривиальные тесты. Их успешное выполнение 
  не гарантирует того, что Ваше решение функционирует правильно.
 ******************************************************************************)

(****************************************************************************** 
  Задание 1 exprToString и pairToString
 ******************************************************************************)

val test1_exprToString = exprToString (VAR "n")  = "VAR \"n\"" 
val test2_exprToString = 
  exprToString (CLOSURE ([("a", INT 6), ("b", INT 9)], INT 7)) 
  = "CLOSURE ([(\"a\", INT 6), (\"b\", INT 9)], INT 7)"
val test3_exprToString = 
  exprToString (PAIR (HEAD NULL, TAIL NULL)) 
  = "PAIR (HEAD (NULL), TAIL (NULL))"
val test4_exprToString = exprToString (PAIR (HEAD NULL, TAIL NULL))
val test5_exprToString = exprToString (FUN (("", "x"), ADD (VAR "a", VAR "x")))  
val test6_exprToString = exprToString (LET (("f", PAIR (HEAD NULL, TAIL NULL)), VAR "52"))
val test7_exprToString = exprToString (IF_GREATER (INT 6, VAR "N", PAIR (HEAD NULL, TAIL NULL), INT 54))


(******************************************************************************)
val test1_pairToString = pairToString ("a", INT 6) = "(\"a\", INT 6)"
(******************************************************************************)

(****************************************************************************** 
  Задание 2 funName
 ******************************************************************************)
val test1_funName = funName (FUN (("", "a"), NULL)) = ""
val test2_funName = ( funName NULL = "QWA"
                        handle Expr => true
                             | _    => false )
val test3_funName = ( funName (ADD (VAR "a", VAR "a")) = "QWA"
                        handle Expr => true
                             | _    => false )
(******************************************************************************)

(****************************************************************************** 
  Задание 3 funArg
 ******************************************************************************)
val test1_funArg = funArg (FUN (("", "a"), NULL)) = "a"
val test2_funArg = ( funArg NULL = "QWA"
                        handle Expr => true
                             | _    => false )
val test3_funArg = ( funArg (ADD (VAR "a", VAR "a")) = "QWA"
                        handle Expr => true
                             | _    => false )
(******************************************************************************)

(****************************************************************************** 
  Задание 4 funBody
 ******************************************************************************)
val test1_funBody = funBody (FUN (("", "a"), NULL)) = NULL
val test2_funBody = ( funBody NULL = NULL
                        handle Expr => true
                             | _    => false )
val test3_funBody = ( funBody (ADD (VAR "a", VAR "a")) = INT 4
                        handle Expr => true
                             | _    => false )
(******************************************************************************)
(****************************************************************************** 
  Задание 5 pairHead
 ******************************************************************************)
val test1_pairHead = pairHead (PAIR (VAR "a", NULL)) = VAR "a"
val test2_pairHead = ( pairHead NULL = NULL
                        handle Expr => true
                             | _    => false )
val test3_pairHead = ( pairHead (ADD (VAR "a", (VAR "a"))) = NULL
                        handle Expr => true
                             | _    => false )
(******************************************************************************)

(****************************************************************************** 
  Задание 6 pairTail
 ******************************************************************************)
val test1_pairTail = pairTail (PAIR (VAR "a", NULL)) = NULL
val test2_pairTail = ( pairTail NULL = NULL
                          handle Expr => true
                               | _    => false )
val test3_pairTail = ( pairTail (ADD (VAR "a", VAR "a")) = NULL
                          handle Expr => true
                               | _    => false )
(******************************************************************************)

(****************************************************************************** 
  Задание 7 closureFun
 ******************************************************************************)
val test1_closureFun = closureFun (CLOSURE ([], FUN (("", "a"), NULL))) 
                        = FUN (("", "a"), NULL)
val test2_closureFun = ( closureFun NULL = NULL 
                            handle Expr => true
                                 | _    => false )
val test3_closureFun = ( closureFun (ADD (VAR "a", VAR "a")) = NULL 
                            handle Expr => true
                                 | _    => false )
(******************************************************************************)

(****************************************************************************** 
  Задание 8 closureEnv
 ******************************************************************************)
val test1_closureEnv = closureEnv (CLOSURE ([], FUN (("", "a"), NULL))) = []
val test2_closureEnv = ( closureEnv NULL = [("a", INT 5)]
                            handle Expr => true
                                 | _    => false )
val test3_closureEnv = ( closureEnv (ADD (VAR "a", VAR "a")) = []
                            handle Expr => true
                                 | _    => false )
(******************************************************************************)

(****************************************************************************** 
  Задание 9 envLookUp
 ******************************************************************************)
val test1_envLookUp = envLookUp ([("a", INT 6), ("b", INT 10)], "b") = INT 10
val test2_envLookUp = ( envLookUp ([("a", INT 6), ("b", INT 10)], "c") = INT 10
                            handle Expr => true
                                 | _    => false )
val test3_envLookUp = ( envLookUp ([], "b") = INT 10
                            handle Expr => true
                                 | _    => false )
(******************************************************************************)

(****************************************************************************** 
  Задание 10 evalUnderEnv
 ******************************************************************************)
val test1_evalUnderEnv = evalUnderEnv (VAR "a") [("a", INT 5)] = INT 5
val test2_evalUnderEnv = evalUnderEnv (INT 5) [] = INT 5
val test3_evalUnderEnv = evalUnderEnv NULL [] = NULL
val test4_evalUnderEnv = evalUnderEnv (ADD (INT 4, VAR "a")) [("a", INT 6), ("l", NULL)]
val test5_evalUnderEnv = evalUnderEnv (CALL (VAR "f", VAR "x"))
[ ("x", INT 4)
, ("a", INT 8)
, ("f", CLOSURE ([("a", INT 6), ("x", INT 10)], FUN (("", "x"), ADD (VAR "a", VAR "x"))))
]
(******************************************************************************)
(****************************************************************************** 
  Задание 11 ifNull
 ******************************************************************************)
val test1_ifNull = evalExp (ifNull (NULL, INT 5, INT 6)) = INT 5
val test2_ifNull = evalExp (ifNull (INT 5, INT 6, NULL)) = NULL
(******************************************************************************)

(****************************************************************************** 
  Задание 12 mLet
 ******************************************************************************)
val test1_mLet = 
  mLet [ ("a", INT 5)
       , ("b", INT 6)
       , ("c", NULL)
       , ("d", PAIR (NULL, NULL))
       ]
       (IF_GREATER (VAR "a", VAR "b", VAR "d", VAR "c"))
  = LET 
      ( ("a", INT 5)
      , LET 
          ( ("b", INT 6)
          , LET 
              ( ("c", NULL)
              , LET 
                  ( ("d", PAIR (NULL, NULL)) 
                  , IF_GREATER ( VAR "a"
                               , VAR "b"
                               , VAR "d"
                               , VAR "c" ) ) ) ) )
val test2_mLet = evalExp ( mLet [ ("a", ADD (INT 1, INT 2))
, ("b", INT 3)
, ("c", INT 5)
]
(IF_GREATER (VAR "a", VAR "b", VAR "c", VAR "a"))
) = INT 3
(******************************************************************************)

(****************************************************************************** 
  Задание 13 ifEq
 ******************************************************************************)
val test1_ifEq = 
  evalExp (ifEq (INT 5, INT 6, NULL, PAIR (NULL, NULL)))
  = PAIR (NULL, NULL) 
val test2_ifEq = evalExp ( ifEq ( ADD (INT 1, INT 2)
                                , INT 3
                                , INT 5
                                , INT 7 )) = INT 5

(******************************************************************************)

(****************************************************************************** 
  Задание 14 convertListToMUPL
 ******************************************************************************)
val test1_convertListToMUPL = convertListToMUPL [] = NULL
val test2_convertListToMUPL = convertListToMUPL [NULL] = PAIR (NULL, NULL)
val test3_convertListToMUPL = convertListToMUPL [VAR "a"] = PAIR (VAR "a", NULL)
val test4_convertListToMUPL = convertListToMUPL [VAR "a", INT 5] = 
  PAIR (VAR "a", PAIR (INT 5, NULL))
(******************************************************************************)

(****************************************************************************** 
  Задание 15 convertListFromMUPL
 ******************************************************************************)
val test1_convertListFromMUPL = convertListFromMUPL NULL = []
val test2_convertListFromMUPL = convertListFromMUPL (PAIR (NULL, NULL)) = [NULL]
val test3_convertListFromMUPL = 
  convertListFromMUPL (PAIR (VAR "a", NULL)) = [(VAR "a")]
val test4_convertListFromMUPL = convertListFromMUPL (PAIR (INT 1, PAIR (INT 2, PAIR (INT 3, PAIR (INT 4, PAIR (INT 5, NULL)))))) =
  [INT 1, INT 2, INT 3, INT 4, INT 5]



(******************************************************************************)

(****************************************************************************** 
  Задание 16 mMap
 ******************************************************************************)
val test1_mMap = 
  evalExp 
    ( CALL 
        ( CALL ( mMap 
               , FUN ( ("", "x") 
                     , ADD (VAR "x", INT 5) ) ) 
        , PAIR (INT 1, PAIR (INT 2, PAIR (INT 3, NULL))) ) )
  = PAIR (INT 6, PAIR (INT 7, PAIR (INT 8, NULL)))
(******************************************************************************)
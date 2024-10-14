(****************************************************************************** 
  Шаблон для выполнения заданий лабораторной работы №2

  НЕ СЛЕДУЕТ УДАЛЯТЬ ИЛИ ПЕРЕСТАВЛЯТЬ МЕСТАМИ ЭЛЕМЕНТЫ, 
  ПРЕДСТАВЛЕННЫЕ В ШАБЛОНЕ (ВКЛЮЧАЯ КОММЕНТАРИИ). 
  ЭЛЕМЕНТЫ РЕШЕНИЯ СЛЕДУЕТ ВПИСЫВАТЬ В ПРОМЕЖУТКИ,
  ОПРЕДЕЛЕННЫЕ КОММЕНТАРИЯМИ.
 ******************************************************************************)

(****************************************************************************** 
  Определение выражений языка MUPL как конструкторов значений типа expr 
 ******************************************************************************)
datatype expr = VAR of string
              | INT of int
              | ADD of expr * expr
              | IF_GREATER of expr * expr * expr * expr
              | FUN of (string * string) * expr
              | CALL of expr * expr
              | LET of (string * expr) * expr
              | PAIR of expr * expr
              | HEAD of expr
              | TAIL of expr
              | NULL 
              | IS_NULL of expr
              | CLOSURE of (string * expr) list * expr
(******************************************************************************)

(****************************************************************************** 
  Описание исключения, которое будет подниматься в случае нарушения семантики 
  выражения языка MUPL
 ******************************************************************************)
exception Expr
(******************************************************************************)

(****************************************************************************** 
  Функция превращения строки в строку, в которой имеются знаки кавычек
 ******************************************************************************)
fun strToString str = String.concat ["\"", str, "\""]
(******************************************************************************)

(****************************************************************************** 
  Задание 1 exprToString и pairToString
 ******************************************************************************)
fun exprToString (VAR s) = String.concat ["VAR ", strToString s]
  | exprToString (INT n) = String.concat ["INT ", Int.toString n]
  | exprToString (ADD (e1, e2)) = String.concat [ "ADD ("
                                                , exprToString e1
                                                , ", "
                                                , exprToString e2
                                                , ")" ]
  | exprToString (IF_GREATER (e1, e2, e3, e4)) = 
      String.concat [ "IF_GREATER ("
                    , exprToString e1
                    , ", "
                    , exprToString e2
                    , ", "
                    , exprToString e3
                    , ", "
                    , exprToString e4
                    , ")" ]
  | exprToString (FUN ((s1, s2), e)) = String.concat [ "FUN (("
                                                     , strToString s1
                                                     , ", "
                                                     , strToString s2
                                                     , "), "
                                                     , exprToString e
                                                     , ")" ]
  | exprToString (CALL (e1, e2)) = String.concat [ "CALL ("
                                                 , exprToString e1
                                                 , ", "
                                                 , exprToString e2
                                                 , ")" ]
  | exprToString (LET ((s, e1), e2)) = String.concat [ "LET (("
                                                     , strToString s
                                                     , ", "
                                                     , exprToString e1
                                                     , "), "
                                                     , exprToString e2
                                                     , ")" ]
  | exprToString (PAIR (e1, e2)) = String.concat [ "PAIR ("
                                                 , exprToString e1
                                                 , ", "
                                                 , exprToString e2
                                                 , ")" ]
  | exprToString (HEAD e) = String.concat [ "HEAD ("
                                          , exprToString e
                                          , ")" ]
  | exprToString (TAIL e) = String.concat ["TAIL (", exprToString e, ")"]
  | exprToString NULL = "NULL"
  | exprToString (IS_NULL e) =
      String.concat ["IS_NULL (", exprToString e, ")"]
  | exprToString (CLOSURE (env, f)) = 
      let
        val nStr = String.concatWith ", " (map pairToString env)
        val bStr = exprToString f
      in
        "CLOSURE ([" ^ nStr ^ "], " ^ bStr ^ ")"
      end
and pairToString (var, expr) = 
  String.concat ["(", strToString var, ", ", exprToString expr, ")"]

(******************************************************************************)

(****************************************************************************** 
  Функция valOfInt
 ******************************************************************************)
fun valOfInt (INT n) = n
  | valOfInt e = 
      ( print ("The expression " ^ exprToString e ^ " is not a number.\n"); 
        raise Expr )
(******************************************************************************)

(****************************************************************************** 
  Задание 2 funName
 ******************************************************************************)
fun funName (FUN ((f, _), _)) = f
  | funName e = 
      ( print ("The expression " ^ exprToString e ^ " is not a function.\n"); 
        raise Expr )

(******************************************************************************)

(****************************************************************************** 
  Задание 3 funArg
 ******************************************************************************)
fun funArg (FUN ((_, x), _)) = x
  | funArg e = 
      ( print ("The expression " ^ exprToString e ^ " is not a function.\n"); 
        raise Expr )


(******************************************************************************)

(****************************************************************************** 
  Задание 4 funBody
 ******************************************************************************)
fun funBody (FUN (_, body)) = body
  | funBody e = 
      ( print ("The expression " ^ exprToString e ^ " is not a function.\n"); 
        raise Expr )


(******************************************************************************)

(****************************************************************************** 
  Задание 5 pairHead
 ******************************************************************************)
fun pairHead (PAIR (e1, _)) = e1
  | pairHead e = 
      ( print ("The expression " ^ exprToString e ^ " is not a pair.\n"); 
        raise Expr )


(******************************************************************************)

(****************************************************************************** 
  Задание 6 pairTail
 ******************************************************************************)
fun pairTail (PAIR (_, e2)) = e2
  | pairTail e = 
      ( print ("The expression " ^ exprToString e ^ " is not a pair.\n"); 
        raise Expr )


(******************************************************************************)

(****************************************************************************** 
  Задание 7 closureFun
 ******************************************************************************)

fun closureFun (CLOSURE (_, f)) = f
  | closureFun e = 
      ( print ("The expression " ^ exprToString e ^ " is not a closure.\n"); 
        raise Expr )

(******************************************************************************)

(****************************************************************************** 
  Задание 8 closureEnv
 ******************************************************************************)
fun closureEnv (CLOSURE (env, _)) = env
  | closureEnv e = 
      ( print ("The expression " ^ exprToString e ^ " is not a closure.\n"); 
        raise Expr )


(******************************************************************************)

(****************************************************************************** 
  Задание 9 envLookUp
 ******************************************************************************)
fun envLookUp ([], str) = 
  (print ("Unbound variable " ^ str ^ ".\n"); raise Expr)
  | envLookUp ((s, v) :: rest, str) = 
      if s = str then v
      else envLookUp (rest, str)

(******************************************************************************)

(****************************************************************************** 
  Задание 10 evalUnderEnv
 ******************************************************************************)
fun evalUnderEnv (INT n) env = INT n
  | evalUnderEnv (VAR x) env = envLookUp (env, x)
  | evalUnderEnv (ADD (e1, e2)) env = 
      INT ( valOfInt (evalUnderEnv e1 env) 
            + valOfInt (evalUnderEnv e2 env) )
  | evalUnderEnv (IF_GREATER (e1, e2, e3, e4)) env = 
      if valOfInt (evalUnderEnv e1 env) > valOfInt (evalUnderEnv e2 env)
      then evalUnderEnv e3 env
      else evalUnderEnv e4 env
  | evalUnderEnv (PAIR (e1, e2)) env = 
      PAIR (evalUnderEnv e1 env, evalUnderEnv e2 env)
  | evalUnderEnv (HEAD e) env = pairHead (evalUnderEnv e env)
  | evalUnderEnv (TAIL e) env = pairTail (evalUnderEnv e env)
  | evalUnderEnv (IS_NULL e) env =
      if (evalUnderEnv e env) = NULL
      then INT 1
      else INT 0
  | evalUnderEnv (LET ((name, e1), e2)) env =
      evalUnderEnv e2 ((name, evalUnderEnv e1 env) :: env)
  | evalUnderEnv (FUN ((name, arg), body)) env =
      CLOSURE (env, FUN ((name, arg), body))
  | evalUnderEnv (CALL (e1, e2)) env =
      let
        val eUE1 = evalUnderEnv e1 env
        val clFunV1 = closureFun eUE1
      in
        evalUnderEnv (funBody clFunV1) ( (funArg clFunV1, evalUnderEnv e2 env)
                                       :: (funName clFunV1, eUE1)
                                       :: closureEnv eUE1 )
      end
  | evalUnderEnv (NULL) _ = NULL
  | evalUnderEnv (CLOSURE (env2, f)) env = CLOSURE (env2, f)

(******************************************************************************)

(****************************************************************************** 
  Функция evalExp
 ******************************************************************************)
fun evalExp expr = evalUnderEnv expr []
(******************************************************************************)

(****************************************************************************** 
  Задание 11 ifNull
 ******************************************************************************)
fun ifNull (e1, e2, e3) = IF_GREATER (IS_NULL e1, INT 0, e2, e3)

(******************************************************************************)

(****************************************************************************** 
  Задание 12 mLet
 ******************************************************************************)
fun mLet [] e = e
  | mLet ((x, v) :: xs) e = LET ((x, v), mLet xs e)

(******************************************************************************)

(****************************************************************************** 
  Задание 13 ifEq
 ******************************************************************************)
fun ifEq (e1, e2, e3, e4) =
  LET ( ("_x", e1)
      , LET ( ("_y", e2)
            , IF_GREATER ( VAR "_x"
                         , VAR "_y"
                         , e4
                         , IF_GREATER ( VAR "_y"
                                      , VAR "_x"
                                      , e4
                                      , e3 ) ) ) )

(******************************************************************************)

(****************************************************************************** 
  Задание 14 convertListToMUPL
 ******************************************************************************)
fun convertListToMUPL [] = NULL
  | convertListToMUPL (x :: xs) = PAIR (x, convertListToMUPL xs)

(******************************************************************************)

(****************************************************************************** 
  Задание 15 convertListFromMUPL
 ******************************************************************************)
fun convertListFromMUPL NULL = []
  | convertListFromMUPL (PAIR (x, xs)) = x :: convertListFromMUPL xs

(******************************************************************************)

(****************************************************************************** 
  Задание 16 mMap
 ******************************************************************************)
val mMap = 
  FUN ( ("func", "arg")
      , FUN ( ("", "lst")
            , ifNull ( VAR "lst"
                     , NULL
                     , PAIR ( CALL (VAR "arg", HEAD (VAR "lst"))
                            , CALL ( CALL (VAR "func", VAR "arg")
                                   , TAIL (VAR "lst") ) )
                     )
            )
      )

(******************************************************************************)

(****************************************************************************** 
  Задание 17 mMapAddN
 ******************************************************************************)
fun mMapAddN (INT n) = 
  CALL (mMap, FUN (("", "x"), ADD (VAR "x", INT n )))
(******************************************************************************)

(****************************************************************************** 
  Задание 18 multAnyXPosY
 ******************************************************************************)
val multAnyXPosY =
  FUN ( ("mult", "x")
      , FUN ( ("", "y")
      , IF_GREATER ( VAR "y"
                   , INT 0
                   , ADD ( VAR "x"
                         , CALL ( CALL (VAR "mult", VAR "x")
                                , ADD (VAR "y", INT ~1) )
                         )
                   , INT 0
                   )
            )
      )

(******************************************************************************)

(****************************************************************************** 
  Задание 19 fact
 ******************************************************************************)


(******************************************************************************)

(****************************************************************************** 
  Задание 20 delDuplicates
 ******************************************************************************)


(******************************************************************************)


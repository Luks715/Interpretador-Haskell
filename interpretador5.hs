newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read)

data Program = Prog Stm
  deriving (Eq, Ord, Show, Read)

data Stm
  = SAss Ident Exp
  | SBlock [Stm]
  | SWhile Exp Stm
  | SdoWhile Stm Exp
  | STry [Stm] [Stm] [Stm]
  deriving (Eq, Ord, Show, Read)

data Exp
  = EOr Exp Exp
  | EAnd Exp Exp
  | ENot Exp
  | ECon Exp Exp
  | EAdd Exp Exp
  | ESub Exp Exp
  | EMul Exp Exp
  | EDiv Exp Exp
  | EInt Integer
  | EVar Ident
  | EStr String
  | ETrue
  | EFalse
  deriving (Eq, Ord, Show, Read)

data Valor
  = ValorStr String
  | ValorInt Integer
  | ValorBool Bool

instance Eq Valor where
  (ValorInt i1) == (ValorInt i2) = i1 == i2
  (ValorStr s1) == (ValorStr s2) = s1 == s2
  (ValorBool b1) == (ValorBool b2) = b1 == b2

s :: Valor -> String
s (ValorStr str) = str

i :: Valor -> Integer
i (ValorInt vint) = vint

b :: Valor -> Bool
b (ValorBool vbool) = vbool

type RContext = [(String, Valor)]
type ErrorMessage = String
--------------------------------------------------------------------------------------
-- As definições deste arquivo são as mínimas para compilar os testes.
-- Você deverá completar todo o restante do código.
-- Dica: se você fez os exercícios anteriores, boa parte do código
-- pode ser reutilizado neste exercício.

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm

execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
  SAss id exp ->
    case eval context exp of
        Right value -> Right (update context (getStr id) value)
        Left err -> Left err

  SBlock [] ->  Right context

  SBlock (s : stms) ->
    case execute context s of
        Right newContext -> execute newContext (SBlock stms)
        Left err -> Left err

  SWhile exp stm ->
    case eval context exp of 
        Right (ValorInt ve1) -> if ve1 == 0
                                then Right context
                                else case execute context stm of 
                                        Right newContext -> execute newContext (SWhile exp stm)
                                        Left msg -> Left msg
        Left msg -> Left msg

  SdoWhile stm exp ->
    case execute context stm of 
        Right newContext -> execute newContext (SWhile exp stm)
        Left  msg -> Left msg

  STry tryStm catchStm finalStm ->
    case tryStm of 
      [] -> case execute context (SBlock finalStm) of 
              Right finalContext -> Right finalContext
              Left msg -> Left msg
      (s : tryRest) -> case execute context s of 
                        Right newContext -> execute newContext (STry tryRest catchStm finalStm)
                        Left msg -> execute context (STry catchStm [] finalStm)

eval :: RContext -> Exp -> Either ErrorMessage Valor
eval context x = case x of

  EOr e1 e2 -> case eval context e1 of
                    Right (ValorBool ve1) -> case eval context e2 of 
                                  Right (ValorBool ve2) -> Right (ValorBool (ve1 || ve2))
                                  Left msg -> Left msg
                    Left msg -> Left msg
  
  EAnd e1 e2 -> case eval context e1 of
                    Right (ValorBool ve1) -> case eval context e2 of 
                                  Right (ValorBool ve2) -> Right (ValorBool (ve1 && ve2))
                                  Left msg -> Left msg
                    Left msg -> Left msg
  
  ENot e1 -> case eval context e1 of
                    Right (ValorBool ve1) -> Right (ValorBool (not ve1))
                    Left msg -> Left msg 

  EAdd e1 e2 -> case eval context e1 of
                    Right (ValorInt ve1) -> case eval context e2 of
                                  Right (ValorInt ve2) -> Right (ValorInt(ve1 + ve2))
                                  Left msg -> Left msg
                    Left msg -> Left msg

  ESub e1 e2 -> case eval context e1 of
                    Right (ValorInt ve1) -> case eval context e2 of
                                  Right (ValorInt ve2) -> Right (ValorInt(ve1 - ve2))
                                  Left msg -> Left msg
                    Left msg -> Left msg

  EMul e1 e2 -> case eval context e1 of
                    Right (ValorInt ve1) -> case eval context e2 of
                                  Right (ValorInt ve2) -> Right (ValorInt(ve1 * ve2))
                                  Left msg -> Left msg
                    Left msg -> Left msg

  EDiv e1 e2 -> case eval context e1 of
                    Right (ValorInt ve1) -> case eval context e2 of
                                  Right (ValorInt ve2) -> if (ve2 == 0)
                                                          then Left ("divisao por 0")
                                                          else Right  (ValorInt(ve1 `div` ve2))
                                  Left msg -> Left msg
                    Left msg -> Left msg

  ECon e1 e2 -> case eval context e1 of
                    Right (ValorStr ve1) -> case eval context e2 of
                                  Right (ValorStr ve2) -> Right (ValorStr (ve1 ++ ve2))
                    Left msg -> Left msg

  EInt n   -> Right (ValorInt n)
  EVar id -> case lookup (getStr id) context of
    Just val -> Right val
    Nothing -> Left $ "Variável " ++ getStr id ++ " não encontrada"
  EStr str -> Right (ValorStr str)
  ETrue    -> Right (ValorBool True)
  EFalse   -> Right (ValorBool False)

------------------------------------------------------------------------------------------
getStr :: Ident -> String
getStr (Ident s) = s

lookupA :: RContext -> String -> Valor 
lookupA ((int,val):cs) str
   | int == str = val
   | otherwise = lookupA cs str

update :: RContext -> String -> Valor -> RContext
update [] str nv = [(str, nv)]
update ((int, val) : cs) str nv
  | int == str = (int, nv) : cs
  | otherwise = (int, val) : update cs str nv
-----------------------------------------------------------------------------------------
main :: IO ()
main = do
    let prog1 =
            Prog
                ( SBlock
                    [ SAss (Ident "x") (EInt 1),
                    SAss (Ident "soma") (EInt 0),
                    SAss (Ident "c") (EInt 10),
                    SdoWhile
                        ( SBlock
                            [ SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EVar (Ident "c"))),
                            SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))
                            ]
                        )
                        (EVar (Ident "c"))
                    ]
            )

    let prog2 =
            Prog
                ( SBlock
                    [ SAss (Ident "x") (EInt 1),
                    SAss (Ident "soma") (EInt 0),
                    SAss (Ident "c") (EInt 1),
                    SdoWhile
                        ( SBlock
                            [ SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EVar (Ident "c"))),
                            SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))
                            ]
                        )
                     (EVar (Ident "c"))
                    ]
                )

    let prog3 =
            Prog
                ( SBlock
                    [ SAss (Ident "w") (EStr "hello"),
                    SAss (Ident "v") (EStr "world"),
                    SAss (Ident "a") (ECon (EVar (Ident "w")) (EVar (Ident "v")))
                    ]
                )
    let prog4 =
            Prog
                ( SBlock
                    [ SAss (Ident "a") ETrue,
                    SAss (Ident "b") EFalse,
                    SAss (Ident "e") ETrue,
                    SAss
                        (Ident "d")
                        ( EOr
                            (ENot (EVar (Ident "a")))
                            (EAnd (EVar (Ident "b")) (EVar (Ident "e")))
                        ),
                    SAss
                        (Ident "f")
                        ( EOr
                            (EAnd (EVar (Ident "a")) (EVar (Ident "b")))
                            (ENot (EVar (Ident "e")))
                        )
                    ]
                )
    let prog5 = Prog (SAss (Ident "x") (EInt 1))
    let prog6 =
            Prog
                ( SBlock
                    [ SAss (Ident "x") (EInt 1),
                    SAss (Ident "soma") (EInt 0),
                    SAss (Ident "c") (EInt 10),
                    SWhile
                        (EVar (Ident "c"))
                        ( SBlock
                            [ SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EVar (Ident "c"))),
                            SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))
                            ]
                        )
                    ]
                )
    let prog7 =
            Prog
                ( SBlock
                    [ SAss (Ident "x") (EInt 1),
                    SAss (Ident "y") (EInt 0),
                    SAss (Ident "z") (EDiv (EVar (Ident "x")) (EVar (Ident "y"))),
                    SAss (Ident "w") (EAdd (EVar (Ident "z")) (EInt 1))
                    ]
                )
    let prog8 =
            Prog
                ( SBlock
                    [ SAss (Ident "x") (EInt 1),
                    SAss (Ident "soma") (EInt 0),
                    SAss (Ident "c") (EInt 10),
                    SWhile
                        (EVar (Ident "c"))
                        ( SBlock
                            [ SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EVar (Ident "c"))),
                            SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))
                            ]
                        )
                    ]
                )
    let prog9 =
            Prog
                ( SBlock
                    [ SAss (Ident "x") (EInt 0),
                    STry
                        [SAss (Ident "soma") (EInt 0), SAss (Ident "c") (EDiv (EVar (Ident "x")) (EVar (Ident "soma")))]
                        [SAss (Ident "x") (EInt 1)]
                        [SAss (Ident "y") (EInt 2)]
                    ]
                )
    let prog10 =
            Prog
                ( SBlock
                    [ SAss (Ident "x") (EInt 0),
                    STry
                        [SAss (Ident "soma") (EInt 10), SAss (Ident "c") (EDiv (EVar (Ident "x")) (EVar (Ident "soma")))]
                        [SAss (Ident "x") (EInt 1)]
                        [SAss (Ident "y") (EInt 2)]
                    ]
                )

    let testCase1 = executeP [] prog1 == Right [("x", ValorInt 1), ("soma", ValorInt 55), ("c", ValorInt 0)]
    let testCase2 = executeP [] prog2 == Right [("x", ValorInt 1), ("soma", ValorInt 1), ("c", ValorInt 0)]
    let testCase3 = executeP [] prog3 == Right [("w", ValorStr "hello"), ("v", ValorStr "world"), ("a", ValorStr "helloworld")]
    let testCase4 =
            executeP [] prog4
                == Right
                [ ("a", ValorBool True),
                    ("b", ValorBool False),
                    ("e", ValorBool True),
                    ("d", ValorBool False),
                    ("f", ValorBool False)
                ]

    let testCase5 = executeP [] prog5 == Right [("x", ValorInt 1)]
    let testCase6 = executeP [] prog6 == Right [("x", ValorInt 1), ("soma", ValorInt 55), ("c", ValorInt 0)]
    let testCase7 = executeP [] prog7 == Left "divisao por 0"
    let testCase8 = executeP [] prog8 == Right [("x", ValorInt 1), ("soma", ValorInt 55), ("c", ValorInt 0)]
    let testCase9 = executeP [] prog9 == Right [("x", ValorInt 1), ("soma", ValorInt 0), ("y", ValorInt 2)]
    let testCase10 = executeP [] prog10 == Right [("x", ValorInt 0), ("soma", ValorInt 10), ("c", ValorInt 0), ("y", ValorInt 2)]

    print testCase1
    print testCase2
    print testCase3
    print testCase4
    print testCase5
    print testCase6
    print testCase7
    print testCase8 
    print testCase9
    print testCase10
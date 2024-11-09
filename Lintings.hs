module Lintings where

import AST
import LintTypes


--------------------------------------------------------------------------------
-- AUXILIARES
--------------------------------------------------------------------------------

-- Computa la lista de variables libres de una expresión
freeVariables :: Expr -> [Name]
freeVariables = undefined


--------------------------------------------------------------------------------
-- LINTINGS
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Computación de constantes
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Reduce expresiones aritméticas/booleanas
-- Construye sugerencias de la forma (LintCompCst e r)
-- lintComputeConstant :: Expr -> (Expr, [LintSugg])
lintComputeConstant :: Linting Expr
lintComputeConstant = \expr -> case expr of
  -- Sumar literales enteros
  Infix Add (Lit (LitInt x)) (Lit (LitInt y)) -> let result = Lit (LitInt (x + y))
                                                 in (result, [LintCompCst expr result])

  -- Restar literales enteros
  Infix Sub (Lit (LitInt x)) (Lit (LitInt y)) -> let result = Lit (LitInt (x - y))
                                                 in (result, [LintCompCst expr result])

  -- Multiplicar literales enteros
  Infix Mult (Lit (LitInt x)) (Lit (LitInt y)) -> let result = Lit (LitInt (x * y))
                                                  in (result, [LintCompCst expr result])

  -- Dividir literales enteros (asumiendo división exacta)
  Infix Div (Lit (LitInt x)) (Lit (LitInt y)) | y /= 0 -> let result = Lit (LitInt (x `div` y))
                                                          in (result, [LintCompCst expr result])

  -- Comparación de igualdad entre literales enteros
  Infix Eq (Lit (LitInt x)) (Lit (LitInt y)) -> let result = Lit (LitBool (x == y))
                                                in (result, [LintCompCst expr result])

  -- Comparación mayor que
  Infix GTh (Lit (LitInt x)) (Lit (LitInt y)) ->
    let result = Lit (LitBool (x > y))
    in (result, [LintCompCst expr result])

  -- Comparación menor que
  Infix LTh (Lit (LitInt x)) (Lit (LitInt y)) ->
    let result = Lit (LitBool (x < y))
    in (result, [LintCompCst expr result])

  -- Operación lógica AND con literales booleanos
  Infix And (Lit (LitBool x)) (Lit (LitBool y)) -> let result = Lit (LitBool (x && y))
                                                   in (result, [LintCompCst expr result])

  -- Operación lógica OR con literales booleanos
  Infix Or (Lit (LitBool x)) (Lit (LitBool y)) -> let result = Lit (LitBool (x || y))
                                                  in (result, [LintCompCst expr result])

  -- Para expresiones compuestas, primero simplificar sus subexpresiones
  Infix op left right -> let (left', leftSugg) = lintComputeConstant left
                             (right', rightSugg) = lintComputeConstant right
                             simplifiedExpr = Infix op left' right'
                         in if simplifiedExpr /= expr then (simplifiedExpr, leftSugg ++ rightSugg)
                            else (expr, leftSugg ++ rightSugg)

  -- Para expresiones que no se pueden simplificar, se devuelven sin cambios
  _ -> (expr, [])


{-
Ejemplo:
String que representa una expresión: "(2 + 2) + (1 + 1)" 
AST correspondiente (este Expr me lo devuelve el Parser que implementaron ellos):
Infix Add (Infix Add (Lit (LitInt 2)) (Lit (LitInt 2)))       
          (Infix Add (Lit (LitInt 1)) (Lit (LitInt 1)))" AST correspondiente
Resultado de la función lintComputeConstant aplicado al Expr:
(Lit (LitInt 6), [LintCompCst (Infix Add (Lit (LitInt 2)) (Lit (LitInt 2))) 
                              (Lit (LitInt 4))
                  ,LintCompCst (Infix Add (Lit (LitInt 1)) (Lit (LitInt 1))) 
                              (Lit (LitInt 2))
                  ,LintCompCst (Infix Add (Lit (LitInt 4)) (Lit (LitInt 2))) 
                              (Lit (LitInt 6))])
Esto es el resultado de aplicar la función (resultado de aplicar las sugerencias y una lista de sugerencias)
-}

--------------------------------------------------------------------------------
-- Eliminación de chequeos redundantes de booleanos
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Elimina chequeos de la forma e == True, True == e, e == False y False == e
-- Construye sugerencias de la forma (LintBool e r)
lintRedBool :: Linting Expr
lintRedBool = undefined


--------------------------------------------------------------------------------
-- Eliminación de if redundantes
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Sustitución de if con literal en la condición por la rama correspondiente
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfCond :: Linting Expr
lintRedIfCond = undefined

--------------------------------------------------------------------------------
-- Sustitución de if por conjunción entre la condición y su rama _then_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfAnd :: Linting Expr
lintRedIfAnd = undefined

--------------------------------------------------------------------------------
-- Sustitución de if por disyunción entre la condición y su rama _else_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfOr :: Linting Expr
lintRedIfOr = undefined

--------------------------------------------------------------------------------
-- Chequeo de lista vacía
--------------------------------------------------------------------------------
-- Sugiere el uso de null para verificar si una lista es vacía
-- Construye sugerencias de la forma (LintNull e r)

lintNull :: Linting Expr
lintNull = undefined

--------------------------------------------------------------------------------
-- Eliminación de la concatenación
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (e:[] ++ es), reemplazando por (e:es)
-- Construye sugerencias de la forma (LintAppend e r)

lintAppend :: Linting Expr
lintAppend = undefined

--------------------------------------------------------------------------------
-- Composición
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (f (g t)), reemplazando por (f . g) t
-- Construye sugerencias de la forma (LintComp e r)

lintComp :: Linting Expr
lintComp = undefined


--------------------------------------------------------------------------------
-- Eta Redución
--------------------------------------------------------------------------------
-- se aplica en casos de la forma \x -> e x, reemplazando por e
-- Construye sugerencias de la forma (LintEta e r)

lintEta :: Linting Expr
lintEta = undefined


--------------------------------------------------------------------------------
-- Eliminación de recursión con map
--------------------------------------------------------------------------------

-- Sustituye recursión sobre listas por `map`
-- Construye sugerencias de la forma (LintMap f r)
lintMap :: Linting FunDef
lintMap = undefined


--------------------------------------------------------------------------------
-- Combinación de Lintings
--------------------------------------------------------------------------------

-- Dada una transformación a nivel de expresión, se construye
-- una transformación a nivel de función
-- liftToFunc :: (Expr -> (Expr, [LintSugg])) -> FunDef -> (FunDef, [LintSugg])
liftToFunc :: Linting Expr -> Linting FunDef
liftToFunc = \lint -> \(FunDef name expr) -> let (newExpr, suggestions) = lint expr
                                             in (FunDef name newExpr, suggestions)

-- encadenar transformaciones:
{-
OBS:
e -> l1 -> e' ls                    linting l1 toma una expresion e y genera una expresion e' y una lista de sugerencias ls
e' -> l2 -> e'' ls'                 linting l2 toma una expresion e' y genera una expresion e'' y una lista de sugerencias ls'
e -> l1 >==> l2 -> e'' (ls ++ ls')    
Es decir que el resultado es la expresión final luego de haber aplicado los lintigs l1 y l2 y el append de las listas de sugerencias que se generaron en orden 
-}
-- (>==>):: Linting a -> Linting a -> Linting a  = (a -> (a, [LintSugg])) -> (a -> (a, [LintSugg])) -> a* -> (a, [LintSugg])
-- a* es el expr que se le pasa como argumento a la lambda expression
(>==>) :: Linting a -> Linting a -> Linting a
lint1 >==> lint2 = \expr -> let (expr1, suggestions1) = lint1 expr -- (expr1, suggestions1) es el resultado de aplicar el primer linting
                                (expr2, suggestions2) = lint2 expr1 -- (expr2, suggestions2) es el resultado de aplicar el segundo linting
                            in (expr2, suggestions1 ++ suggestions2) -- retorno de la expresion final y la lista de sugerencias que se generaron en orden


-- aplica las transformaciones 'lints' repetidas veces y de forma incremental,
-- hasta que ya no generen más cambios en 'func'
{-
lintRec :: (a -> (a, [LintSugg])) -> a -> (a, [LintSugg])
lints es (a -> (a, [LintSugg])) y func es a (a puede ser una expresion o una funcion)
-}
lintRec :: Linting a -> Linting a
lintRec lints func = let (newFunc, suggestions) = lints func
                     in if (length suggestions == 0) then (func, suggestions)  -- si no hay cambios, devolvemos el resultado actual
                        else let (finalFunc, finalSuggestions) = lintRec lints newFunc
                             in (finalFunc, suggestions ++ finalSuggestions)  -- acumulamos las sugerencias

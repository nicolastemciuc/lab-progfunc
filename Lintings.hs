{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
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

   -- Suma de literales enteros (solo si el resultado no es negativo)
  Infix Add (Lit (LitInt x)) (Lit (LitInt y)) -> let result = x + y
                                                 in if result >= 0 then let resultExpr = Lit (LitInt result) 
                                                                        in  (resultExpr, [LintCompCst expr resultExpr])
                                                    else (expr, [])

  -- Resta de literales enteros (solo si el resultado no es negativo)
  Infix Sub (Lit (LitInt x)) (Lit (LitInt y)) -> let result = x - y
                                                 in if result >= 0 then let resultExpr = Lit (LitInt result) 
                                                                        in  (resultExpr, [LintCompCst expr resultExpr])
                                                    else (expr, [])

  -- Multiplicación de literales enteros (solo si el resultado no es negativo)
  Infix Mult (Lit (LitInt x)) (Lit (LitInt y)) -> let result = x * y
                                                  in if result >= 0 then let resultExpr = Lit (LitInt result) 
                                                                        in  (resultExpr, [LintCompCst expr resultExpr])
                                                     else (expr, [])

  -- División de literales enteros (solo si el divisor no es 0 y resultado no es negativo)
  Infix Div (Lit (LitInt x)) (Lit (LitInt y)) -> if y == 0 then (expr, [])
                                                 else let result = x `div` y
                                                      in if result >= 0 then let resultExpr = Lit (LitInt result) 
                                                                             in  (resultExpr, [LintCompCst expr resultExpr])
                                                         else (expr, [])
  
  -- Comparación de igualdad entre literales enteros
  Infix Eq (Lit (LitInt x)) (Lit (LitInt y)) -> let result = Lit (LitBool (x == y))
                                                in (result, [LintCompCst expr result])

  -- Comparación mayor que
  Infix GTh (Lit (LitInt x)) (Lit (LitInt y)) -> let result = Lit (LitBool (x > y))
                                                 in (result, [LintCompCst expr result])

  -- Comparación menor que
  Infix LTh (Lit (LitInt x)) (Lit (LitInt y)) -> let result = Lit (LitBool (x < y))
                                                 in (result, [LintCompCst expr result])

  -- Operación lógica AND entre literales booleanos
  Infix And (Lit (LitBool x)) (Lit (LitBool y)) -> let result = Lit (LitBool (x && y))
                                                   in (result, [LintCompCst expr result])

  -- Operación lógica OR entre literales booleanos
  Infix Or (Lit (LitBool x)) (Lit (LitBool y)) -> let result = Lit (LitBool (x || y))
                                                  in (result, [LintCompCst expr result])

  -- Para expresiones compuestas, simplificar sus subexpresiones de izquierda a derecha
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
lintRedBool = \expr -> case expr of

  -- Operacion logica de igualdad entre un literal booleano y variable
  Infix Eq (Lit (LitBool x)) y -> if x then (y, [LintBool expr y])
                                        else let result = App (Var "not") y
                                        in (result, [LintBool expr result])

  -- Operacion logica de igualdad entre un literal booleano y variable
  Infix Eq x (Lit (LitBool y)) -> if y then (x, [LintBool expr x])
                                        else let result = App (Var "not") x
                                        in (result, [LintBool expr result])

  -- Para expresiones compuestas, aplicar el linting en subexpresiones de izquierda a derecha
  Infix op left right ->
    let (left', leftSugg) = lintRedBool left
        (right', rightSugg) = lintRedBool right
        simplifiedExpr = Infix op left' right'
    in if simplifiedExpr /= expr
       then (simplifiedExpr, leftSugg ++ rightSugg)
       else (expr, leftSugg ++ rightSugg)

  -- Para expresiones que no se pueden simplificar, se devuelven sin cambios
  _ -> (expr, [])



--------------------------------------------------------------------------------
-- Eliminación de if redundantes
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Sustitución de if con literal en la condición por la rama correspondiente
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfCond :: Linting Expr
lintRedIfCond = \expr -> case expr of

  -- Si la condición es True, devolver la rama _then_
  If (Lit (LitBool True)) x _ -> (x, [LintRedIf expr x])

  -- Si la condición es False, devolver la rama _else_
  If (Lit (LitBool False)) _ y -> (y, [LintRedIf expr y])

  -- Para expresiones compuestas, aplicar el linting en subexpresiones de izquierda a derecha
  Infix op left right ->
    let (left', leftSugg) = lintRedIfCond left
        (right', rightSugg) = lintRedIfCond right
        simplifiedExpr = Infix op left' right'
    in if simplifiedExpr /= expr
       then (simplifiedExpr, leftSugg ++ rightSugg)
       else (expr, leftSugg ++ rightSugg)

  -- Para expresiones que no se pueden simplificar, se devuelven sin cambios
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Sustitución de if por conjunción entre la condición y su rama _then_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfAnd :: Linting Expr
lintRedIfAnd = \expr -> case expr of

  -- Si la rama then es True
  If c (Lit (LitBool True)) e ->
    let result = Infix Or c e
    in (result, [LintRedIf expr result])

  -- Si la rama then es False
  If c (Lit (LitBool False)) e ->
    let result = Infix And (App (Var "not") c) e
    in (result, [LintRedIf expr result])

  -- Para expresiones compuestas, aplicar el linting en subexpresiones de izquierda a derecha
  Infix op left right ->
    let (left', leftSugg) = lintRedIfAnd left
        (right', rightSugg) = lintRedIfAnd right
        simplifiedExpr = Infix op left' right'
    in if simplifiedExpr /= expr
       then (simplifiedExpr, leftSugg ++ rightSugg)
       else (expr, leftSugg ++ rightSugg)

  -- Para expresiones que no se pueden simplificar, se devuelven sin cambios
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Sustitución de if por disyunción entre la condición y su rama _else_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfOr :: Linting Expr
lintRedIfOr = \expr -> case expr of

  -- Si la rama else es True
  If c t (Lit (LitBool True)) ->
    let result = Infix Or (App(Var "not") c) t
    in (result, [LintRedIf expr result])

  -- Si la rama else es False
  If c t (Lit (LitBool False)) ->
    let result = Infix And c t
    in (result, [LintRedIf expr result])

  -- Para expresiones compuestas, aplicar el linting en subexpresiones de izquierda a derecha
  Infix op left right ->
    let (left', leftSugg) = lintRedIfOr left
        (right', rightSugg) = lintRedIfOr right
        simplifiedExpr = Infix op left' right'
    in if simplifiedExpr /= expr
       then (simplifiedExpr, leftSugg ++ rightSugg)
       else (expr, leftSugg ++ rightSugg)

  -- Para expresiones que no se pueden simplificar, se devuelven sin cambios
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Chequeo de lista vacía 
--------------------------------------------------------------------------------
-- Sugiere el uso de null para verificar si una lista es vacía
-- Construye sugerencias de la forma (LintNull e r)
-- listNull :: Expr -> (Expr, [LintSugg])
lintNull :: Linting Expr
lintNull expr = case expr of

  -- Caso: e == []
  Infix Eq e (Lit LitNil) ->
    let (e', eSugg) = lintNull e
        result = App (Var "null") e'
        expr2 = Infix Eq e' (Lit LitNil)
    in (result, eSugg ++ [LintNull expr2 result])

  -- Caso: [] == e (simbólico)
  Infix Eq (Lit LitNil) e ->
    let (e', eSugg) = lintNull e
        result = App (Var "null") e'
        expr2 = Infix Eq (Lit LitNil) e'
    in (result, eSugg ++ [LintNull expr2 result])

  -- Caso: length e == 0
  Infix Eq (App (Var "length") e) (Lit (LitInt 0)) ->
    let (e', eSugg) = lintNull e
        result = App (Var "null") e'
        expr2 = App (Var "length") e'
    in (result, eSugg ++ [LintNull expr result])

  -- Caso: 0 == length e (simbólico)
  Infix Eq (Lit (LitInt 0)) (App (Var "length") e) ->
    let (e', eSugg) = lintNull e
        result = App (Var "null") e'
        expr2 = App (Var "length") e'
    in (result, eSugg ++ [LintNull expr2 result])

  -- Para expresiones compuestas, simplificar subexpresiones en orden de izquierda a derecha
  Infix op left right ->
    let (left', leftSugg) = lintNull left
        (right', rightSugg) = lintNull right
        simplifiedExpr = Infix op left' right'
    in if simplifiedExpr /= expr
       then (simplifiedExpr, leftSugg ++ rightSugg)
       else (expr, leftSugg ++ rightSugg)

  -- Para expresiones que no cumplen los patrones anteriores, devolver sin cambios
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Eliminación de la concatenación
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (e:[] ++ es), reemplazando por (e:es)
-- Construye sugerencias de la forma (LintAppend e r)

lintAppend :: Linting Expr
lintAppend expr = case expr of

  -- Caso: e : [] ++ es
  Infix Append (Infix Cons e (Lit LitNil)) es ->
    let (e', eSugg) = lintAppend e
        (es', esSugg) = lintAppend es
        result = Infix Cons e' es'
        expr2 = Infix Cons e' (Infix Append (Lit LitNil) es')
    in (result, eSugg ++ esSugg ++ [LintAppend expr2 result])

  -- Para expresiones compuestas, aplicar recursión en subexpresiones
  Infix op left right ->
    let (left', leftSugg) = lintAppend left
        (right', rightSugg) = lintAppend right
        simplifiedExpr = Infix op left' right'
    in if simplifiedExpr /= expr
       then (simplifiedExpr, leftSugg ++ rightSugg)
       else (expr, leftSugg ++ rightSugg)

  -- Para expresiones que no coinciden con el patrón, devolver sin cambios
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Composición
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (f (g t)), reemplazando por (f . g) t
-- Construye sugerencias de la forma (LintComp e r)
-- f (g x) = (f . g) x
-- f (g (h x)) = (f . (g . h)) x
lintComp :: Linting Expr
lintComp expr = case expr of
  -- Caso: f (g x) -> (f . g) x
  App f (App g x) ->
    let (f', eSugg1) = lintComp f    
        (g', eSugg2) = lintComp g    
        (x', eSugg3) = lintComp x    
    in case x' of
         -- Si x es de la forma (h z)
         App h z -> 
           let (y, eSugg4) = lintComp (App g (App h z))
               (result, eSugg5) = lintComp (App f y)
           in (result, eSugg4 ++ eSugg5)

         -- Caso base de composición: f (g x) -> (f . g) x, x es solo una x
         _ -> 
           let result = App (Infix Comp f' g') x'
               expr2 = App f' (App g' x') 
           in (result, eSugg1 ++ eSugg2 ++ eSugg3 ++ [LintComp expr2 result])

  -- Para cualquier otro patrón, devolver la expresión sin cambios
  _ -> (expr, [])

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

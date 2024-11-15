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
freeVariables expr = case expr of
  -- Caso base: una variable `Var` es libre
  Var x -> [x]

  -- Literales no tienen variables libres
  Lit _ -> []

  -- En una expresión infija, las variables libres son las libres de ambas partes
  Infix _ e1 e2 -> freeVariables e1 ++ freeVariables e2

  -- En una aplicación, las variables libres son las de ambas subexpresiones
  App e1 e2 -> freeVariables e1 ++ freeVariables e2

  -- En una expresión lambda `Lam x e`, `x` queda ligada, entonces la removemos
  Lam x e -> filter (/= x) (freeVariables e)

  -- En una expresión `Case e1 e2 (x, y, e3)`, computamos las libres en `e1` y `e2` y, en `e3`,
  -- quitamos `x` y `y` porque son patrones de una expresión case (ligados).
  Case e1 e2 (x, y, e3) ->
    freeVariables e1 ++ freeVariables e2 ++ filter (\v -> v /= x && v /= y) (freeVariables e3)

  -- En una expresión condicional `If e1 e2 e3`, las libres son las de cada subexpresión
  If e1 e2 e3 -> freeVariables e1 ++ freeVariables e2 ++ freeVariables e3


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
lintComputeConstant expr = case expr of

  -- Suma de literales enteros (solo si el resultado no es negativo)
  Infix Add e1 e2 ->
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      -- Si después de la recursión ambos son literales, calcular el resultado
      (Lit (LitInt x), Lit (LitInt y)) ->
        let result = x + y
            resultExpr = Lit (LitInt result)
            expr2 = Infix Add (Lit (LitInt x)) (Lit (LitInt y))
        in if result >= 0 
           then (resultExpr, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 resultExpr])
           else (expr2, e1Sugg ++ e2Sugg)
      -- De lo contrario, devolver la expresión simplificada hasta ahora
      _ -> (Infix Add e1' e2', e1Sugg ++ e2Sugg)

                        
  -- Resta de literales enteros (solo si el resultado no es negativo)
  Infix Sub e1 e2 ->
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      -- Si después de la recursión ambos son literales, calcular el resultado
      (Lit (LitInt x), Lit (LitInt y)) ->
        let result = x - y
            resultExpr = Lit (LitInt result)
            expr2 = Infix Sub (Lit (LitInt x)) (Lit (LitInt y))
        in if result >= 0 
           then (resultExpr, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 resultExpr])
           else (expr2, e1Sugg ++ e2Sugg)
      -- De lo contrario, devolver la expresión simplificada hasta ahora
      _ -> (Infix Sub e1' e2', e1Sugg ++ e2Sugg)


  -- Multiplicación de literales enteros (solo si el resultado no es negativo)
  Infix Mult e1 e2 ->
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      -- Si después de la recursión ambos son literales, calcular el resultado
      (Lit (LitInt x), Lit (LitInt y)) ->
        let result = x * y
            resultExpr = Lit (LitInt result)
            expr2 = Infix Mult (Lit (LitInt x)) (Lit (LitInt y))
        in if result >= 0 
           then (resultExpr, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 resultExpr])
           else (expr2, e1Sugg ++ e2Sugg)
      -- De lo contrario, devolver la expresión simplificada hasta ahora
      _ -> (Infix Mult e1' e2', e1Sugg ++ e2Sugg)
  
  -- División de literales enteros (solo si el divisor no es 0 y resultado no es negativo)
  Infix Div e1 e2 ->
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      -- Si después de la recursión ambos son literales, calcular el resultado
      (Lit (LitInt x), Lit (LitInt y)) ->
            if y /= 0 then let result = x `div` y 
                               resultExpr = Lit (LitInt result)
                               expr2 = Infix Div (Lit (LitInt x)) (Lit (LitInt y))
                            in if result >= 0 
                               then (resultExpr, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 resultExpr])
                               else (expr2, e1Sugg ++ e2Sugg)
            else let expr3 = Infix Div (Lit (LitInt x)) (Lit (LitInt y))
                 in (expr3, e1Sugg ++ e2Sugg) 
              
      -- De lo contrario, devolver la expresión simplificada hasta ahora
      _ -> (Infix Div e1' e2', e1Sugg ++ e2Sugg)
  

  -- Operación lógica AND entre literales booleanos
  Infix And e1 e2 -> let (e1', e1Sugg) = lintComputeConstant e1
                         (e2', e2Sugg) = lintComputeConstant e2
                          in case (e1', e2') of
                            -- Si después de la recursión ambos son literales, calcular el resultado
                            (Lit (LitBool x), Lit (LitBool y)) ->
                              let result = Lit (LitBool (x && y))
                                  expr2 = Infix And (Lit (LitBool x)) (Lit (LitBool y))
                              in (result, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 result])
                            -- De lo contrario, devolver la expresión simplificada hasta ahora
                            _ -> (Infix And e1' e2', e1Sugg ++ e2Sugg)


  -- Operación lógica OR entre literales booleanos
  Infix Or e1 e2 -> let (e1', e1Sugg) = lintComputeConstant e1
                        (e2', e2Sugg) = lintComputeConstant e2
                        in case (e1', e2') of
                          -- Si después de la recursión ambos son literales, calcular el resultado
                          (Lit (LitBool x), Lit (LitBool y)) ->
                            let result = Lit (LitBool (x || y))
                                expr2 = Infix Or (Lit (LitBool x)) (Lit (LitBool y))
                            in (result, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 result])
                          -- De lo contrario, devolver la expresión simplificada hasta ahora
                          _ -> (Infix Or e1' e2', e1Sugg ++ e2Sugg)

  -- Comparación de igualdad entre literales enteros
  Infix Eq e1 e2 -> 
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      -- Si después de la recursión ambos son literales, calcular el resultado
      (Lit (LitInt x), Lit (LitInt y)) ->
        let result = Lit (LitBool (x == y))
            expr2 = Infix Eq (Lit (LitInt x)) (Lit (LitInt y))
        in (result, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 result])
      -- De lo contrario, devolver la expresión simplificada hasta ahora
      _ -> (Infix Eq e1' e2', e1Sugg ++ e2Sugg)

  -- Comparación mayor que entre literales enteros
  Infix GTh e1 e2 -> 
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      -- Si después de la recursión ambos son literales, calcular el resultado
      (Lit (LitInt x), Lit (LitInt y)) ->
        let result = Lit (LitBool (x > y))
            expr2 = Infix GTh (Lit (LitInt x)) (Lit (LitInt y))
        in (result, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 result])
      -- De lo contrario, devolver la expresión simplificada hasta ahora
      _ -> (Infix GTh e1' e2', e1Sugg ++ e2Sugg)

  -- Comparación menor que entre literales enteros
  Infix LTh e1 e2 -> 
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      -- Si después de la recursión ambos son literales, calcular el resultado
      (Lit (LitInt x), Lit (LitInt y)) ->
        let result = Lit (LitBool (x < y))
            expr2 = Infix LTh (Lit (LitInt x)) (Lit (LitInt y))
        in (result, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 result])
      -- De lo contrario, devolver la expresión simplificada hasta ahora
      _ -> (Infix LTh e1' e2', e1Sugg ++ e2Sugg)

  -- Recursión en subexpresiones
  Infix op left right -> 
    let (left', leftSugg) = lintComputeConstant left
        (right', rightSugg) = lintComputeConstant right
        simplifiedExpr = Infix op left' right'
    in if simplifiedExpr /= expr 
       then (simplifiedExpr, leftSugg ++ rightSugg)
       else (expr, leftSugg ++ rightSugg)

  App e1 e2 -> 
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in (App e1' e2', e1Sugg ++ e2Sugg)
  
  Lam x body -> 
    let (body', bodySugg) = lintComputeConstant body
    in (Lam x body', bodySugg)

  Case e1 e2 (x, y, e3) ->
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
        (e3', e3Sugg) = lintComputeConstant e3
    in (Case e1' e2' (x, y, e3'), e1Sugg ++ e2Sugg ++ e3Sugg)

  If e1 e2 e3 -> 
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
        (e3', e3Sugg) = lintComputeConstant e3
    in (If e1' e2' e3', e1Sugg ++ e2Sugg ++ e3Sugg)

  -- Caso general para expresiones que no se simplifican
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Eliminación de chequeos redundantes de booleanos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Elimina chequeos de la forma e == True, True == e, e == False y False == e
-- Construye sugerencias de la forma (LintBool e r)
-- lintRedBool :: Expr -> (Expr, [LintSugg])
lintRedBool :: Linting Expr
lintRedBool = \expr -> case expr of

  -- Caso e == True -> e
  Infix Eq e (Lit (LitBool True)) ->  let (e', eSugg) = lintRedBool e
                                          expr2 = Infix Eq e' (Lit (LitBool True))
                                      in (e', eSugg ++ [LintBool expr2 e'])
    
  -- Caso True == e -> e
  Infix Eq (Lit (LitBool True)) e ->  let (e', eSugg) = lintRedBool e
                                          expr2 = Infix Eq (Lit (LitBool True)) e'
                                      in (e', eSugg ++ [LintBool expr2 e'])

  -- Caso e == False -> not e
  Infix Eq e (Lit (LitBool False)) -> let (e', eSugg) = lintRedBool e
                                          expr2 = Infix Eq e' (Lit (LitBool False))
                                          result = App (Var "not") e'
                                      in (result, eSugg ++ [LintBool expr2 result])
                   

  -- Caso False == e -> not e
  Infix Eq (Lit (LitBool False)) e -> let (e', eSugg) = lintRedBool e
                                          expr2 = Infix Eq (Lit (LitBool False)) e'
                                          result = App (Var "not") e'
                                      in (result, eSugg ++ [LintBool expr2 result])

  -- Recursión en subexpresiones
  Infix op left right ->
    let (left', leftSugg) = lintRedBool left
        (right', rightSugg) = lintRedBool right
        simplifiedExpr = Infix op left' right'
    in if simplifiedExpr /= expr
       then (simplifiedExpr, leftSugg ++ rightSugg)
       else (expr, leftSugg ++ rightSugg)

  App e1 e2 ->
    let (e1', e1Sugg) = lintRedBool e1
        (e2', e2Sugg) = lintRedBool e2
    in (App e1' e2', e1Sugg ++ e2Sugg)
  
  Lam x body ->
    let (body', bodySugg) = lintRedBool body
    in (Lam x body', bodySugg)

  Case e1 e2 (x, y, e3) ->
    let (e1', e1Sugg) = lintRedBool e1
        (e2', e2Sugg) = lintRedBool e2
        (e3', e3Sugg) = lintRedBool e3
    in (Case e1' e2' (x, y, e3'), e1Sugg ++ e2Sugg ++ e3Sugg)

  If e1 e2 e3 ->
    let (e1', e1Sugg) = lintRedBool e1
        (e2', e2Sugg) = lintRedBool e2
        (e3', e3Sugg) = lintRedBool e3
    in (If e1' e2' e3', e1Sugg ++ e2Sugg ++ e3Sugg)
  
  -- Para expresiones que no se pueden simplificar, se devuelven sin cambios
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Eliminación de if redundantes
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Sustitución de if con literal en la condición por la rama correspondiente
-- Construye sugerencias de la forma (LintRedIf e r)
-- lintRedIfCond :: Expr -> (Expr, [LintSugg])
lintRedIfCond :: Linting Expr
lintRedIfCond expr = case expr of
  
  -- Caso if True then t else e -> t
  If (Lit (LitBool True)) t e -> let (t', sugg) = lintRedIfCond t
                                     (e', sugg') = lintRedIfCond e
                                     expr2 = If (Lit (LitBool True)) t' e'
                                  in (t', sugg ++ sugg' ++ [LintRedIf expr2 t'])
    

  -- Caso if False then t else e -> e
  If (Lit (LitBool False)) t e -> let (t', sugg) = lintRedIfCond t
                                      (e', sugg') = lintRedIfCond e
                                      expr2 = If (Lit (LitBool False)) t' e'
                                  in (e', sugg ++ sugg' ++ [LintRedIf expr2 e'])

  -- Recursión en subexpresiones
  If c e t -> let (c', cSugg) = lintRedIfAnd c
                  (e', eSugg) = lintRedIfAnd e
                  (t', tSugg) = lintRedIfAnd t
                  expr2 = If c' e' t'
              in (expr2, cSugg ++ eSugg ++ tSugg)

  Infix op left right -> 
    let (left', leftSugg) = lintRedIfCond left
        (right', rightSugg) = lintRedIfCond right
    in (Infix op left' right', leftSugg ++ rightSugg)

  App e1 e2 ->
    let (e1', e1Sugg) = lintRedIfCond e1
        (e2', e2Sugg) = lintRedIfCond e2
    in (App e1' e2', e1Sugg ++ e2Sugg)

  Lam x body ->
    let (body', bodySugg) = lintRedIfCond body
    in (Lam x body', bodySugg)

  Case e1 e2 (x, y, e3) ->
    let (e1', e1Sugg) = lintRedIfCond e1
        (e2', e2Sugg) = lintRedIfCond e2
        (e3', e3Sugg) = lintRedIfCond e3
    in (Case e1' e2' (x, y, e3'), e1Sugg ++ e2Sugg ++ e3Sugg)

  -- Caso base, devolver la expresión sin cambios
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Sustitución de if por conjunción entre la condición y su rama _then_
-- Construye sugerencias de la forma (LintRedIf e r)
-- lintRedIfAnd :: Expr -> (Expr, [LintSugg])
lintRedIfAnd :: Linting Expr
lintRedIfAnd expr = case expr of
  
  -- Caso if c then e else False -> c && e
  If c e (Lit (LitBool False)) -> let (c', cSugg) = lintRedIfAnd c
                                      (e', eSugg) = lintRedIfAnd e
                                      expr2 = If c' e' (Lit (LitBool False)) 
                                      result = Infix And c' e'
                                  in (result, cSugg ++ eSugg ++ [LintRedIf expr2 result])

  -- Recursión en subexpresiones
  If c e t -> let (c', cSugg) = lintRedIfAnd c
                  (e', eSugg) = lintRedIfAnd e
                  (t', tSugg) = lintRedIfAnd t
                  expr2 = If c' e' t'
              in (expr2, cSugg ++ eSugg ++ tSugg)

  Infix op left right -> 
    let (left', leftSugg) = lintRedIfAnd left
        (right', rightSugg) = lintRedIfAnd right
    in (Infix op left' right', leftSugg ++ rightSugg)

  App e1 e2 ->
    let (e1', e1Sugg) = lintRedIfAnd e1
        (e2', e2Sugg) = lintRedIfAnd e2
    in (App e1' e2', e1Sugg ++ e2Sugg)

  Lam x body ->
    let (body', bodySugg) = lintRedIfAnd body
    in (Lam x body', bodySugg)

  Case e1 e2 (x, y, e3) ->
    let (e1', e1Sugg) = lintRedIfAnd e1
        (e2', e2Sugg) = lintRedIfAnd e2
        (e3', e3Sugg) = lintRedIfAnd e3
    in (Case e1' e2' (x, y, e3'), e1Sugg ++ e2Sugg ++ e3Sugg)

  -- Caso base, devolver la expresión sin cambios
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Sustitución de if por disyunción entre la condición y su rama _else_
-- Construye sugerencias de la forma (LintRedIf e r)
-- lintRedIfOr :: Expr -> (Expr, [LintSugg])
lintRedIfOr :: Linting Expr
lintRedIfOr expr = case expr of

    -- if c then True else e -> c || e
  If c (Lit (LitBool True)) e -> let (c', cSugg) = lintRedIfOr c
                                     (e', eSugg) = lintRedIfOr e
                                     expr2 = If c' (Lit (LitBool True)) e' 
                                     result = Infix Or c' e'
                                     in (result, cSugg ++ eSugg ++ [LintRedIf expr2 result])

  -- Recursión en subexpresione
  If c e t -> let (c', cSugg) = lintRedIfAnd c
                  (e', eSugg) = lintRedIfAnd e
                  (t', tSugg) = lintRedIfAnd t
                  expr2 = If c' e' t'
              in (expr2, cSugg ++ eSugg ++ tSugg)

  Infix op left right -> 
    let (left', leftSugg) = lintRedIfOr left
        (right', rightSugg) = lintRedIfOr right
    in (Infix op left' right', leftSugg ++ rightSugg)

  App e1 e2 ->
    let (e1', e1Sugg) = lintRedIfOr e1
        (e2', e2Sugg) = lintRedIfOr e2
    in (App e1' e2', e1Sugg ++ e2Sugg)

  Lam x body ->
    let (body', bodySugg) = lintRedIfOr body
    in (Lam x body', bodySugg)

  Case e1 e2 (x, y, e3) ->
    let (e1', e1Sugg) = lintRedIfOr e1
        (e2', e2Sugg) = lintRedIfOr e2
        (e3', e3Sugg) = lintRedIfOr e3
    in (Case e1' e2' (x, y, e3'), e1Sugg ++ e2Sugg ++ e3Sugg)

  -- Caso base, devolver la expresión sin cambios
  _ -> (expr, [])
--------------------------------------------------------------------------------
-- Chequeo de lista vacía 
--------------------------------------------------------------------------------
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
        expr2 = Infix Eq (App (Var "length") e') (Lit (LitInt 0))
    in (result, eSugg ++ [LintNull expr2 result])

  -- Caso: 0 == length e (simbólico)
  Infix Eq (Lit (LitInt 0)) (App (Var "length") e) -> 
    let (e', eSugg) = lintNull e
        result = App (Var "null") e'
        expr2 = Infix Eq (Lit (LitInt 0)) (App (Var "length") e')
    in (result, eSugg ++ [LintNull expr2 result])

  -- Casos generales: recorrer recursivamente en orden de izquierda a derecha
  Infix op left right -> 
    let (left', leftSugg) = lintNull left
        (right', rightSugg) = lintNull right
        simplifiedExpr = Infix op left' right'
    in (simplifiedExpr, leftSugg ++ rightSugg)

  App e1 e2 -> 
    let (e1', e1Sugg) = lintNull e1
        (e2', e2Sugg) = lintNull e2
    in (App e1' e2', e1Sugg ++ e2Sugg)

  Lam x body -> 
    let (body', bodySugg) = lintNull body
    in (Lam x body', bodySugg)

  Case e1 e2 (x, y, e3) -> 
    let (e1', e1Sugg) = lintNull e1
        (e2', e2Sugg) = lintNull e2
        (e3', e3Sugg) = lintNull e3
    in (Case e1' e2' (x, y, e3'), e1Sugg ++ e2Sugg ++ e3Sugg)

  If e1 e2 e3 -> 
    let (e1', e1Sugg) = lintNull e1
        (e2', e2Sugg) = lintNull e2
        (e3', e3Sugg) = lintNull e3
    in (If e1' e2' e3', e1Sugg ++ e2Sugg ++ e3Sugg)

  -- Otros patrones no son modificados, se devuelven sin cambios
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Eliminación de la concatenación
--------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- se aplica en casos de la forma (e:[] ++ es), reemplazando por (e:es)
-- Construye sugerencias de la forma (LintAppend e r)
-- lintAppend :: Expr -> (Expr, [LintSugg])
lintAppend :: Linting Expr
lintAppend expr = case expr of

  -- Caso principal: e : [] ++ es -> e : es
  Infix Append (Infix Cons e (Lit LitNil)) es ->
    let (e', eSugg) = lintAppend e
        (es', esSugg) = lintAppend es
        result = Infix Cons e' es'
        expr2 = Infix Append (Infix Cons e' (Lit LitNil)) es'
    in (result, eSugg ++ esSugg ++ [LintAppend expr2 result])

  -- Recursión en operadores infijos, aplicando lintAppend de izquierda a derecha
  Infix op left right ->
    let (left', leftSugg) = lintAppend left
        (right', rightSugg) = lintAppend right
    in (Infix op left' right', leftSugg ++ rightSugg)

  -- Recursión en aplicaciones
  App e1 e2 ->
    let (e1', e1Sugg) = lintAppend e1
        (e2', e2Sugg) = lintAppend e2
    in (App e1' e2', e1Sugg ++ e2Sugg)

  -- Recursión en lambdas
  Lam x body ->
    let (body', bodySugg) = lintAppend body
    in (Lam x body', bodySugg)

  -- Recursión en case
  Case e1 e2 (x, y, e3) ->
    let (e1', e1Sugg) = lintAppend e1
        (e2', e2Sugg) = lintAppend e2
        (e3', e3Sugg) = lintAppend e3
    in (Case e1' e2' (x, y, e3'), e1Sugg ++ e2Sugg ++ e3Sugg)
  
  If e1 e2 e3 ->
    let (e1', e1Sugg) = lintAppend e1
        (e2', e2Sugg) = lintAppend e2
        (e3', e3Sugg) = lintAppend e3
    in (If e1' e2' e3', e1Sugg ++ e2Sugg ++ e3Sugg)

  -- Otros casos: devolver la expresión sin cambios
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Composición
--------------------------------------------------------------------------------
----------------------------------------------------------------------------
-- se aplica en casos de la forma (f (g t)), reemplazando por (f . g) t
-- Construye sugerencias de la forma (LintComp e r)
-- f (g x) = (f . g) x
-- f (g (h x)) = (f . (g . h)) x
-- f (g (h (i x))) = (f . (g . (h . i))) x
-- lintComp :: Expr -> (Expr, [LintSugg])
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
----------------------------------------------------------------------------
-- se aplica en casos de la forma \x -> e x, reemplazando por e
-- Construye sugerencias de la forma (LintEta e r)
-- lintEta :: Expr -> (Expr, [LintSugg])
lintEta :: Linting Expr
lintEta expr = case expr of
  -- Caso de eta-reducción: \x -> e x se convierte en e si x no está libre en e
  Lam x (App e (Var v)) ->
    let (reducedE, eSugg) = lintEta e                -- Reducir `e` primero, si es posible
        originalExpr = Lam x (App reducedE (Var x))  -- Expresión original antes de la reducción
    in if v == x && notElem x (freeVariables reducedE) 
       then (reducedE, eSugg ++ [LintEta originalExpr reducedE])   -- Aplicar eta-reducción
       else (Lam x (App reducedE (Var v)), eSugg)                  -- No se puede reducir más
  
  -- Aplicar lintEta recursivamente dentro de lambdas
  Lam x body ->
    let (body', bodySugg) = lintEta body
    in (Lam x body', bodySugg)

  -- Aplicar lintEta recursivamente en aplicaciones
  App e1 e2 ->
    let (e1', e1Sugg) = lintEta e1
        (e2', e2Sugg) = lintEta e2
    in (App e1' e2', e1Sugg ++ e2Sugg)

  -- Aplicar lintEta en expresiones infijas
  Infix op e1 e2 ->
    let (e1', e1Sugg) = lintEta e1
        (e2', e2Sugg) = lintEta e2
    in (Infix op e1' e2', e1Sugg ++ e2Sugg)

  Case e1 e2 (x, y, e3) ->
    let (e1', e1Sugg) = lintEta e1
        (e2', e2Sugg) = lintEta e2
        (e3', e3Sugg) = lintEta e3
    in (Case e1' e2' (x, y, e3'), e1Sugg ++ e2Sugg ++ e3Sugg)

  If e1 e2 e3 ->
    let (e1', e1Sugg) = lintEta e1
        (e2', e2Sugg) = lintEta e2
        (e3', e3Sugg) = lintEta e3
    in (If e1' e2' e3', e1Sugg ++ e2Sugg ++ e3Sugg)

  -- Para cualquier otro patrón, devolver la expresión sin cambios
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Eliminación de recursión con map
--------------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Sustituye recursión sobre listas por `map`
-- Construye sugerencias de la forma (LintMap f r)
-- lintMap :: FunDef -> (FunDef, [LintSugg])
lintMap :: Linting FunDef
lintMap (FunDef funcName expr) = case expr of
  -- Detectamos si la función sigue el esquema `\l -> case l of ...`
  Lam l (Case (Var l') baseCase (x, xs, recursiveCase)) ->
    if l == l' && baseCase == Lit LitNil
    then case recursiveCase of
      -- Detectamos el caso recursivo `(x : xs) -> e : func xs`
      Infix Cons e (App (Var funcName') (Var xs')) ->
        if funcName == funcName' && xs == xs' && all (`notElem` freeVariables e) [funcName, xs, l]
        then
          let -- Construimos la expresión `map (\x -> e) l`
              transformedExpr = App (Var "map") (Lam x e)
              newFunDef = FunDef funcName transformedExpr
              suggestion = LintMap (FunDef funcName expr) newFunDef
          in (newFunDef, [suggestion])
        else (FunDef funcName expr, [])
      -- Si el caso recursivo no coincide, devolvemos la función sin cambios
      _ -> (FunDef funcName expr, [])
    else (FunDef funcName expr, [])

  -- Si no se encuentra un patrón que coincida, devolvemos la función sin cambios
  _ -> (FunDef funcName expr, [])

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
-- lintRec :: (a -> (a, [LintSugg])) -> a -> (a, [LintSugg])
-- lints es (a -> (a, [LintSugg])) y func es a (a puede ser una expresion o una funcion)
lintRec :: Linting a -> Linting a
lintRec lints func = let (newFunc, suggestions) = lints func
                     in if (length suggestions == 0) then (func, suggestions)  -- si no hay cambios, devolvemos el resultado actual
                        else let (finalFunc, finalSuggestions) = lintRec lints newFunc
                             in (finalFunc, suggestions ++ finalSuggestions)  -- acumulamos las sugerencias
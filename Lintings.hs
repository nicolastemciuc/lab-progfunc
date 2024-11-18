module Lintings where

import AST
import LintTypes

--------------------------------------------------------------------------------
-- AUXILIARES
--------------------------------------------------------------------------------

-- Computa la lista de variables libres de una expresión
freeVariables :: Expr -> [Name]
freeVariables expr = case expr of
  Var x -> [x]
  Lit _ -> []
  Infix _ e1 e2 -> freeVariables e1 ++ freeVariables e2
  App e1 e2 -> freeVariables e1 ++ freeVariables e2
  Lam x e -> filter (/= x) (freeVariables e) -- En una expresión lambda x queda ligada, entonces la removemos de las variables libres de e
  Case e1 e2 (x, xs, e3) -> freeVariables e1 ++ freeVariables e2 ++ filter (\v -> v /= x && v /= xs) (freeVariables e3) -- En una expresión case x y xs quedan ligadas, entonces las removemos de las variables libres de e3
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
      -- Si después de la recursión ambas expresiones son literales hacemos la operación
      (Lit (LitInt x), Lit (LitInt y)) ->
        let result = x + y
            resultExpr = Lit (LitInt result)
            expr2 = Infix Add (Lit (LitInt x)) (Lit (LitInt y))
        in if result >= 0 
           then (resultExpr, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 resultExpr])
           else (expr2, e1Sugg ++ e2Sugg)
      -- Sino devolvemos la expresión simplificada hasta ahora
      _ -> (Infix Add e1' e2', e1Sugg ++ e2Sugg)
       
  -- Resta de literales enteros (solo si el resultado no es negativo)
  Infix Sub e1 e2 ->
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      (Lit (LitInt x), Lit (LitInt y)) ->
        let result = x - y
            resultExpr = Lit (LitInt result)
            expr2 = Infix Sub (Lit (LitInt x)) (Lit (LitInt y))
        in if result >= 0 
           then (resultExpr, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 resultExpr])
           else (expr2, e1Sugg ++ e2Sugg)
      _ -> (Infix Sub e1' e2', e1Sugg ++ e2Sugg)


  -- Multiplicación de literales enteros (solo si el resultado no es negativo)
  Infix Mult e1 e2 ->
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      (Lit (LitInt x), Lit (LitInt y)) ->
        let result = x * y
            resultExpr = Lit (LitInt result)
            expr2 = Infix Mult (Lit (LitInt x)) (Lit (LitInt y))
        in if result >= 0 
           then (resultExpr, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 resultExpr])
           else (expr2, e1Sugg ++ e2Sugg)
      _ -> (Infix Mult e1' e2', e1Sugg ++ e2Sugg)
  
  -- División de literales enteros (solo si el divisor no es 0 y resultado no es negativo)
  Infix Div e1 e2 ->
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      (Lit (LitInt x), Lit (LitInt y)) ->
            if y /= 0 then let result = x `div` y 
                               resultExpr = Lit (LitInt result)
                               expr2 = Infix Div (Lit (LitInt x)) (Lit (LitInt y))
                            in if result >= 0 
                               then (resultExpr, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 resultExpr])
                               else (expr2, e1Sugg ++ e2Sugg)
            else let expr3 = Infix Div (Lit (LitInt x)) (Lit (LitInt y))
                 in (expr3, e1Sugg ++ e2Sugg) 
      _ -> (Infix Div e1' e2', e1Sugg ++ e2Sugg)
  
  -- Operación lógica AND entre literales booleanos
  Infix And e1 e2 -> 
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      -- Si después de la recursión ambas expresiones son literales hacemos la operación
      (Lit (LitBool x), Lit (LitBool y)) ->
        let result = Lit (LitBool (x && y))
            expr2 = Infix And (Lit (LitBool x)) (Lit (LitBool y))
        in (result, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 result])
      -- Sino devolvemos la expresión simplificada hasta ahora
      _ -> (Infix And e1' e2', e1Sugg ++ e2Sugg)


  -- Operación lógica OR entre literales booleanos
  Infix Or e1 e2 -> 
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
        in case (e1', e2') of
          (Lit (LitBool x), Lit (LitBool y)) ->
            let result = Lit (LitBool (x || y))
                expr2 = Infix Or (Lit (LitBool x)) (Lit (LitBool y))
            in (result, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 result])
          _ -> (Infix Or e1' e2', e1Sugg ++ e2Sugg)

  -- Comparación de igualdad entre literales enteros
  Infix Eq e1 e2 -> 
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      -- Si después de la recursión ambas expresiones son literales hacemos la operación
      (Lit (LitInt x), Lit (LitInt y)) ->
        let result = Lit (LitBool (x == y))
            expr2 = Infix Eq (Lit (LitInt x)) (Lit (LitInt y))
        in (result, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 result])
      -- Sino devolvemos la expresión simplificada hasta ahora
      _ -> (Infix Eq e1' e2', e1Sugg ++ e2Sugg)

  -- Comparación mayor que entre literales enteros
  Infix GTh e1 e2 -> 
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      (Lit (LitInt x), Lit (LitInt y)) ->
        let result = Lit (LitBool (x > y))
            expr2 = Infix GTh (Lit (LitInt x)) (Lit (LitInt y))
        in (result, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 result])
      _ -> (Infix GTh e1' e2', e1Sugg ++ e2Sugg)

  -- Comparación menor que entre literales enteros
  Infix LTh e1 e2 -> 
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in case (e1', e2') of
      (Lit (LitInt x), Lit (LitInt y)) ->
        let result = Lit (LitBool (x < y))
            expr2 = Infix LTh (Lit (LitInt x)) (Lit (LitInt y))
        in (result, e1Sugg ++ e2Sugg ++ [LintCompCst expr2 result])
      _ -> (Infix LTh e1' e2', e1Sugg ++ e2Sugg)

  ------ Recursion en subexpresiones ------
  Infix op e1 e2 -> 
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
        simplifiedExpr = Infix op e1' e2'
    in if simplifiedExpr /= expr 
       then (simplifiedExpr, e1Sugg ++ e2Sugg)
       else (expr, e1Sugg ++ e2Sugg)

  App e1 e2 -> 
    let (e1', e1Sugg) = lintComputeConstant e1
        (e2', e2Sugg) = lintComputeConstant e2
    in (App e1' e2', e1Sugg ++ e2Sugg)
  
  Lam n e -> 
    let (e', eSugg) = lintComputeConstant e
    in (Lam n e', eSugg)

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

  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Eliminación de chequeos redundantes de booleanos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Elimina chequeos de la forma e == True, True == e, e == False y False == e
-- Construye sugerencias de la forma (LintBool e r)
-- lintRedBool :: Expr -> (Expr, [LintSugg])
lintRedBool :: Linting Expr
lintRedBool expr = case expr of

  -- Caso e == True -> e
  Infix Eq e (Lit (LitBool True)) -> 
    let (e', eSugg) = lintRedBool e
        expr2 = Infix Eq e' (Lit (LitBool True))
    in (e', eSugg ++ [LintBool expr2 e'])
    
  -- Caso True == e -> e
  Infix Eq (Lit (LitBool True)) e ->  
    let (e', eSugg) = lintRedBool e
        expr2 = Infix Eq (Lit (LitBool True)) e'
    in (e', eSugg ++ [LintBool expr2 e'])

  -- Caso e == False -> not e
  Infix Eq e (Lit (LitBool False)) ->
    let (e', eSugg) = lintRedBool e
        expr2 = Infix Eq e' (Lit (LitBool False))
        result = App (Var "not") e'
    in (result, eSugg ++ [LintBool expr2 result])
                   
  -- Caso False == e -> not e
  Infix Eq (Lit (LitBool False)) e -> 
    let (e', eSugg) = lintRedBool e
        expr2 = Infix Eq (Lit (LitBool False)) e'
        result = App (Var "not") e'
    in (result, eSugg ++ [LintBool expr2 result])

  ------ Recursión en subexpresiones ------
  Infix op e1 e2 -> 
    let (e1', e1Sugg) = lintRedBool e1
        (e2', e2Sugg) = lintRedBool e2
        simplifiedExpr = Infix op e1' e2'
    in if simplifiedExpr /= expr 
       then (simplifiedExpr, e1Sugg ++ e2Sugg)
       else (expr, e1Sugg ++ e2Sugg)
  
  App e1 e2 ->
    let (e1', e1Sugg) = lintRedBool e1
        (e2', e2Sugg) = lintRedBool e2
    in (App e1' e2', e1Sugg ++ e2Sugg)

  Lam n e ->
    let (e', eSugg) = lintRedBool e
    in (Lam n e', eSugg)
  
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
  If (Lit (LitBool True)) t e -> 
    let (t', tSugg) = lintRedIfCond t
        (e', eSugg) = lintRedIfCond e
        expr2 = If (Lit (LitBool True)) t' e'
    in (t', tSugg ++ eSugg ++ [LintRedIf expr2 t'])
    
  -- Caso if False then t else e -> e
  If (Lit (LitBool False)) t e -> 
    let (t', tSugg) = lintRedIfCond t
        (e', eSugg) = lintRedIfCond e
        expr2 = If (Lit (LitBool False)) t' e'
    in (e', tSugg ++ eSugg ++ [LintRedIf expr2 e'])

  ------ Recursión en subexpresiones ------
  Infix op e1 e2 -> 
    let (e1', e1Sugg) = lintRedIfCond e1
        (e2', e2Sugg) = lintRedIfCond e2
        simplifiedExpr = Infix op e1' e2'
    in if simplifiedExpr /= expr 
       then (simplifiedExpr, e1Sugg ++ e2Sugg)
       else (expr, e1Sugg ++ e2Sugg)
  
  App e1 e2 ->
    let (e1', e1Sugg) = lintRedIfCond e1
        (e2', e2Sugg) = lintRedIfCond e2
    in (App e1' e2', e1Sugg ++ e2Sugg)
  
  Lam n e ->
    let (e', eSugg) = lintRedIfCond e
    in (Lam n e', eSugg)
  
  Case e1 e2 (x, y, e3) ->
    let (e1', e1Sugg) = lintRedIfCond e1
        (e2', e2Sugg) = lintRedIfCond e2
        (e3', e3Sugg) = lintRedIfCond e3
    in (Case e1' e2' (x, y, e3'), e1Sugg ++ e2Sugg ++ e3Sugg)
  
  If e1 e2 e3 ->
    let (e1', e1Sugg) = lintRedIfCond e1
        (e2', e2Sugg) = lintRedIfCond e2
        (e3', e3Sugg) = lintRedIfCond e3
    in (If e1' e2' e3', e1Sugg ++ e2Sugg ++ e3Sugg)
  
  _ -> (expr, [])
  
--------------------------------------------------------------------------------
-- Sustitución de if por conjunción entre la condición y su rama _then_
-- Construye sugerencias de la forma (LintRedIf e r)
-- lintRedIfAnd :: Expr -> (Expr, [LintSugg])
lintRedIfAnd :: Linting Expr
lintRedIfAnd expr = case expr of
  
  -- Caso if c then e else False -> c && e
  If c e (Lit (LitBool False)) -> 
    let (c', cSugg) = lintRedIfAnd c
        (e', eSugg) = lintRedIfAnd e
        expr2 = If c' e' (Lit (LitBool False)) 
        result = Infix And c' e'
    in (result, cSugg ++ eSugg ++ [LintRedIf expr2 result])

  ------ Recursión en subexpresiones ------
  Infix op e1 e2 -> 
    let (e1', e1Sugg) = lintRedIfAnd e1
        (e2', e2Sugg) = lintRedIfAnd e2
        simplifiedExpr = Infix op e1' e2'
    in if simplifiedExpr /= expr 
       then (simplifiedExpr, e1Sugg ++ e2Sugg)
       else (expr, e1Sugg ++ e2Sugg)

  App e1 e2 ->
    let (e1', e1Sugg) = lintRedIfAnd e1
        (e2', e2Sugg) = lintRedIfAnd e2
    in (App e1' e2', e1Sugg ++ e2Sugg)
  
  Lam n e ->
    let (e', eSugg) = lintRedIfAnd e
    in (Lam n e', eSugg)
  
  Case e1 e2 (x, y, e3) ->
    let (e1', e1Sugg) = lintRedIfAnd e1
        (e2', e2Sugg) = lintRedIfAnd e2
        (e3', e3Sugg) = lintRedIfAnd e3
    in (Case e1' e2' (x, y, e3'), e1Sugg ++ e2Sugg ++ e3Sugg)
  
  If e1 e2 e3 ->
    let (e1', e1Sugg) = lintRedIfAnd e1
        (e2', e2Sugg) = lintRedIfAnd e2
        (e3', e3Sugg) = lintRedIfAnd e3
    in (If e1' e2' e3', e1Sugg ++ e2Sugg ++ e3Sugg)

  _ -> (expr, [])
    
--------------------------------------------------------------------------------
-- Sustitución de if por disyunción entre la condición y su rama _else_
-- Construye sugerencias de la forma (LintRedIf e r)
-- lintRedIfOr :: Expr -> (Expr, [LintSugg])
lintRedIfOr :: Linting Expr
lintRedIfOr expr = case expr of

    -- if c then True else e -> c || e
  If c (Lit (LitBool True)) e -> 
    let (c', cSugg) = lintRedIfOr c
        (e', eSugg) = lintRedIfOr e
        expr2 = If c' (Lit (LitBool True)) e' 
        result = Infix Or c' e'
    in (result, cSugg ++ eSugg ++ [LintRedIf expr2 result])

  ------ Recursión en subexpresiones ------
  Infix op e1 e2 -> 
    let (e1', e1Sugg) = lintRedIfOr e1
        (e2', e2Sugg) = lintRedIfOr e2
        simplifiedExpr = Infix op e1' e2'
    in if simplifiedExpr /= expr 
       then (simplifiedExpr, e1Sugg ++ e2Sugg)
       else (expr, e1Sugg ++ e2Sugg)
  
  App e1 e2 ->
    let (e1', e1Sugg) = lintRedIfOr e1
        (e2', e2Sugg) = lintRedIfOr e2
    in (App e1' e2', e1Sugg ++ e2Sugg)
  
  Lam n e ->
    let (e', eSugg) = lintRedIfOr e
    in (Lam n e', eSugg)
  
  Case e1 e2 (x, y, e3) ->
    let (e1', e1Sugg) = lintRedIfOr e1
        (e2', e2Sugg) = lintRedIfOr e2
        (e3', e3Sugg) = lintRedIfOr e3
    in (Case e1' e2' (x, y, e3'), e1Sugg ++ e2Sugg ++ e3Sugg)
  
  If e1 e2 e3 ->
    let (e1', e1Sugg) = lintRedIfOr e1
        (e2', e2Sugg) = lintRedIfOr e2
        (e3', e3Sugg) = lintRedIfOr e3
    in (If e1' e2' e3', e1Sugg ++ e2Sugg ++ e3Sugg)
  
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

  ------ Recursión en subexpresiones ------
  Infix op e1 e2 -> 
    let (e1', eSugg) = lintNull e1
        (e2', eSugg2) = lintNull e2
        simplifiedExpr = Infix op e1' e2'
    in if simplifiedExpr /= expr 
       then (simplifiedExpr, eSugg ++ eSugg2)
       else (expr, eSugg ++ eSugg2)
    
  App e1 e2 ->
    let (e1', eSugg) = lintNull e1
        (e2', eSugg2) = lintNull e2
    in (App e1' e2', eSugg ++ eSugg2)
  
  Lam n e ->
    let (e', eSugg) = lintNull e
    in (Lam n e', eSugg)

  Case e1 e2 (x, y, e3) ->
    let (e1', eSugg) = lintNull e1
        (e2', eSugg2) = lintNull e2
        (e3', eSugg3) = lintNull e3
    in (Case e1' e2' (x, y, e3'), eSugg ++ eSugg2 ++ eSugg3)
  
  If e1 e2 e3 ->
    let (e1', eSugg) = lintNull e1
        (e2', eSugg2) = lintNull e2
        (e3', eSugg3) = lintNull e3
    in (If e1' e2' e3', eSugg ++ eSugg2 ++ eSugg3)

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

  ---- Recursión en subexpresiones ----
  Infix op e1 e2 -> 
    let (e1', eSugg) = lintAppend e1
        (e2', eSugg2) = lintAppend e2
        simplifiedExpr = Infix op e1' e2'
    in if simplifiedExpr /= expr 
       then (simplifiedExpr, eSugg ++ eSugg2)
       else (expr, eSugg ++ eSugg2)

  App e1 e2 ->
    let (e1', eSugg) = lintAppend e1
        (e2', eSugg2) = lintAppend e2
    in (App e1' e2', eSugg ++ eSugg2)
  
  Lam n e ->
    let (e', eSugg) = lintAppend e
    in (Lam n e', eSugg)
  
  Case e1 e2 (x, y, e3) ->
    let (e1', eSugg) = lintAppend e1
        (e2', eSugg2) = lintAppend e2
        (e3', eSugg3) = lintAppend e3
    in (Case e1' e2' (x, y, e3'), eSugg ++ eSugg2 ++ eSugg3)
  
  If e1 e2 e3 ->
    let (e1', eSugg) = lintAppend e1
        (e2', eSugg2) = lintAppend e2
        (e3', eSugg3) = lintAppend e3
    in (If e1' e2' e3', eSugg ++ eSugg2 ++ eSugg3)
  
  _ -> (expr, [])

--------------------------------------------------------------------------------
-- Composición
--------------------------------------------------------------------------------
----------------------------------------------------------------------------
-- se aplica en casos de la forma (f (g t)), reemplazando por (f . g) t
-- Construye sugerencias de la forma (LintComp e r)
-- f (g x) = (f . g) x
-- f (g (h x)) = (f . (g . h)) x
-- f (g (h (j x))) = (f . (g . (h . j))) x
-- lintComp :: Expr -> (Expr, [LintSugg])
lintComp :: Linting Expr
lintComp expr = case expr of
  -- Caso: f (g x) -> (f . g) x
  App f (App g x) ->
    let (f', eSugg1) = lintComp f
        (g', eSugg2) = lintComp g
    in case x of
         -- Caso base: f (g x) -> (f . g) x, x es solo una x
         Var _ ->
           let result = App (Infix Comp f' g') x
               expr2 = App f' (App g' x)
           in (result, eSugg1 ++ eSugg2 ++ [LintComp expr2 result])
        -- Caso recursivo: f (g (h x)) -> f ( (g . h) x) -> (f . (g . h)) x
         _ -> let (App h z, eSugg3) = lintComp (App g' x)
                  result = App (Infix Comp f' h) z
                  expr2 = App f' (App h z)
              in (result, eSugg1 ++ eSugg2 ++ eSugg3 ++ [LintComp expr2 result])

  ---- Recursión en subexpresiones ----
  Infix op e1 e2 -> 
    let (e1', eSugg) = lintComp e1
        (e2', eSugg2) = lintComp e2
        simplifiedExpr = Infix op e1' e2'
    in if simplifiedExpr /= expr 
       then (simplifiedExpr, eSugg ++ eSugg2)
       else (expr, eSugg ++ eSugg2)
  
  App e1 e2 ->
    let (e1', eSugg) = lintComp e1
        (e2', eSugg2) = lintComp e2
    in (App e1' e2', eSugg ++ eSugg2)
  
  Lam n e ->
    let (e', eSugg) = lintComp e
    in (Lam n e', eSugg)
  
  Case e1 e2 (x, y, e3) ->
    let (e1', eSugg) = lintComp e1
        (e2', eSugg2) = lintComp e2
        (e3', eSugg3) = lintComp e3
    in (Case e1' e2' (x, y, e3'), eSugg ++ eSugg2 ++ eSugg3)
  
  If e1 e2 e3 ->
    let (e1', eSugg) = lintComp e1
        (e2', eSugg2) = lintComp e2
        (e3', eSugg3) = lintComp e3
    in (If e1' e2' e3', eSugg ++ eSugg2 ++ eSugg3)

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
  
  ---- Recursión en subexpresiones ----
  Infix op e1 e2 -> 
    let (e1', eSugg) = lintEta e1
        (e2', eSugg2) = lintEta e2
        simplifiedExpr = Infix op e1' e2'
    in if simplifiedExpr /= expr 
       then (simplifiedExpr, eSugg ++ eSugg2)
       else (expr, eSugg ++ eSugg2)
  
  App e1 e2 ->
    let (e1', eSugg) = lintEta e1
        (e2', eSugg2) = lintEta e2
    in (App e1' e2', eSugg ++ eSugg2)

  Lam n e ->
    let (e', eSugg) = lintEta e
    in (Lam n e', eSugg)
  
  Case e1 e2 (x, y, e3) ->
    let (e1', eSugg) = lintEta e1
        (e2', eSugg2) = lintEta e2
        (e3', eSugg3) = lintEta e3
    in (Case e1' e2' (x, y, e3'), eSugg ++ eSugg2 ++ eSugg3)
  
  If e1 e2 e3 ->
    let (e1', eSugg) = lintEta e1
        (e2', eSugg2) = lintEta e2
        (e3', eSugg3) = lintEta e3
    in (If e1' e2' e3', eSugg ++ eSugg2 ++ eSugg3)
  
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
  -- Vemos si expr es de la forma \l -> case l of ...
  Lam l (Case (Var l') baseCase (x, xs, recursiveCase)) ->
    if l == l' && baseCase == Lit LitNil
    then case recursiveCase of
      -- Detectamos el caso recursivo (x : xs) -> e : func xs
      Infix Cons e (App (Var funcName') (Var xs')) ->
        if funcName == funcName' && xs == xs' && all (`notElem` freeVariables e) [funcName, xs, l]
        then
          let -- Construimos la expresión map (\x -> e) l
              transformedExpr = App (Var "map") (Lam x e)
              newFunDef = FunDef funcName transformedExpr
              suggestion = LintMap (FunDef funcName expr) newFunDef
          in (newFunDef, [suggestion])
        else (FunDef funcName expr, [])
      -- Si el caso recursivo no coincide, devolvemos la función sin cambios
      _ -> (FunDef funcName expr, [])
    else (FunDef funcName expr, [])

  -- Si no es de la forma esperada, devolvemos la función sin cambios
  _ -> (FunDef funcName expr, [])

--------------------------------------------------------------------------------
-- Combinación de Lintings
--------------------------------------------------------------------------------

-- Dada una transformación a nivel de expresión, se construye
-- una transformación a nivel de función
-- liftToFunc :: (Expr -> (Expr, [LintSugg])) -> FunDef -> (FunDef, [LintSugg])
liftToFunc :: Linting Expr -> Linting FunDef
liftToFunc lint (FunDef name expr) = let (newExpr, suggestions) = lint expr
                                     in (FunDef name newExpr, suggestions)

-- encadenar transformaciones:
-- (>==>):: Linting a -> Linting a -> Linting a  = (a -> (a, [LintSugg])) -> (a -> (a, [LintSugg])) -> a* -> (a, [LintSugg])
-- a* es el expr que se le pasa como argumento a la lambda expression
(>==>) :: Linting a -> Linting a -> Linting a
lint1 >==> lint2 = \expr -> 
  let (expr1, suggestions1) = lint1 expr -- (expr1, suggestions1) es el resultado de aplicar el primer linting
      (expr2, suggestions2) = lint2 expr1 -- (expr2, suggestions2) es el resultado de aplicar el segundo linting
  in (expr2, suggestions1 ++ suggestions2) -- retornamos la expresión final y la lista de sugerencias que se generaron en orden

-- aplica las transformaciones 'lints' repetidas veces y de forma incremental,
-- hasta que ya no generen más cambios en 'func'
-- lintRec :: (a -> (a, [LintSugg])) -> a -> (a, [LintSugg])
lintRec :: Linting a -> Linting a
lintRec lints func = let (newFunc, suggestions) = lints func
                     in if (length suggestions == 0) then (func, suggestions)  -- si no hay cambios, devolvemos el resultado actual
                        else let (finalFunc, finalSuggestions) = lintRec lints newFunc
                             in (finalFunc, suggestions ++ finalSuggestions)  -- acumulamos las sugerencias

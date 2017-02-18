{-# LANGUAGE Strict, StrictData #-}

module SECD.Compiler (compile) where

import SECD.Internal
import Prelude hiding (EQ)

compile :: Lisp -> Either String Code
compile = compile' []

compile' :: [String] -> Lisp -> Either String Code
compile' rs lisp = case lisp of
  LAtom "debug" -> Right $ [DEBUG]
  LAtom a -> Right $ [LD a]
  LList ls' -> case ls' of
    [] -> Right $ [LDC $ LList []]
    LAtom "let"    : ls -> let' rs ls
    LAtom "letrec" : ls -> letrec rs ls
    LAtom "lam"    : ls -> lam rs ls
    LAtom "if"     : ls -> if' rs ls
    LAtom "list"   : ls -> list rs ls
    LAtom "eq"     : ls -> eq rs ls
    LAtom "atom"   : ls -> atom rs ls
    LAtom "cons"   : ls -> cons rs ls
    LAtom "car"    : ls -> car rs ls
    LAtom "cdr"    : ls -> cdr rs ls
    LAtom "+"      : ls -> add rs ls
    LAtom "puts"   : ls -> puts rs ls
    LAtom "do"     : ls -> do' rs ls
    _ -> apply rs ls'
  _ -> Right $ [LDC lisp]

let' rs ls = case ls of
  LList [LAtom name, expr] : body -> do
    let rs' = filter (/= name) rs
    e <- compile' rs' expr
    b <- concat <$> mapM (compile' rs') body 
    pure $ e ++ [LET name] ++ b
  _ ->
    Left $ "syntax error let: " ++ show ls
    
letrec rs ls = case ls of
  LList [LAtom name, expr] : body -> do
    let rs' = name : rs
    e <- compile' rs' expr
    b <- concat <$> mapM (compile' rs') body
    pure $ e ++ [LET name] ++ b
  _ ->
    Left $ "syntax error letrec: " ++ show ls

lam rs ls = case ls of
  [LList args, body] ->
    (\a -> [LDF (map (\(LAtom a) -> a) args) $ a ++ [RET]]) <$> compile' rs body
  [LAtom args, body] ->
    (\a -> [LDF [args] $ a ++ [RET]]) <$> compile' rs body
  _ ->
    Left $ "syntax error lam: " ++ show ls

if' rs ls = case ls of
  [b, t, f] -> do
    b' <- compile' rs b
    t' <- compile' rs t
    f' <- compile' rs f
    pure $ b' ++ [SEL (t' ++ [JOIN]) (f' ++ [JOIN])]
  _ ->
    Left $ "syntax error if: " ++ show ls

list rs ls = case ls of
  list ->
    ((++ [LIST $ length list]) . concat) <$> mapM (compile' rs) ls

eq rs ls = case ls of
  [a, b] ->
    (++ [EQ]) <$> ((++) <$> compile' rs b <*> compile' rs a)
  _ -> 
    Left $ "syntax error eq: " ++ show ls

atom rs ls = case ls of
  [a] -> 
    (++ [ATOM]) <$> compile' rs a
  _ ->
    Left $ "syntax error atom: " ++ show ls


cons rs ls = case ls of
  [a, b] ->
    (++ [CONS]) <$> ((++) <$> compile' rs b <*> compile' rs a)
  _ -> Left $ "syntax error cons: " ++ show ls

car rs ls = case ls of
  [a] ->
    (++ [CAR]) <$> compile' rs a
  _ ->
    Left $ "syntax error car: " ++ show ls

cdr rs ls = case ls of
  [a] ->
    (++ [CDR]) <$> compile' rs a 
  _ ->
    Left $ "syntax error cdr: " ++ show ls

add rs ls = case ls of 
  [a,b] ->
    (++ [ADD]) <$> ((++) <$> compile' rs b <*> compile' rs a)
  _ ->
    Left $ "syntax error +: " ++ show ls

puts rs ls = case ls of 
  [a] -> 
    (++ [PUTS]) <$> compile' rs a 
  _ ->
    Left $ "syntax error puts: " ++ show ls

do' rs ls = 
  concat <$> mapM (compile' rs) ls

apply rs (lam : args) = do
  args' <- mapM (compile' rs) $ reverse args
  lam' <- compile' rs lam
  pure $ concat args' ++ [LIST $ length args] ++ lam' ++ ap
  where
    ap = case lam of 
      LAtom name -> if elem name rs then [RAP] else [AP]
      _ -> [AP]

module SECD.Internal where

import Prelude hiding (EQ)
import qualified Data.Map as M
import Data.List (intersperse)

data SECD 
  = SECD {
    stack :: Stack,
    env   :: Env,
    code  :: Code,
    dump  :: Dump
  } deriving Show

type Stack = [Lisp]
type Env = M.Map String Lisp
type Code = [CodeOP]
type Dump = [DumpOP]

data CodeOP 
  = LET String
  | LD String
  | LDC Lisp
  | LDF [String] Code
  | LIST Int
  | AP
  | RET
  | RAP
  | SEL Code Code
  | JOIN
  | CONS
  | CAR
  | CDR
  | EQ
  | ATOM
  | ADD
  | PUTS 
  | DEBUG
  | STOP
  deriving Show

data DumpOP 
  = DumpAP Stack Env Code
  | DumpSEL Code
  deriving Show

data Lisp
  = LList [Lisp]
  | LAtom String
  | LNum Int
  | LTrue
  | LFalse
  | LNil
  | LCons Lisp Lisp
  | LClosure [String] Code Env
  | LError String

instance Eq Lisp where
  LList a == LList b = a == b
  LCons a b == LCons c d = a == c && b == d
  LAtom a == LAtom b = a == b
  LNum a  == LNum b  = a == b
  LTrue   == LTrue   = True
  LFalse  == LFalse  = True
  LNil    == LNil    = True
  _       == _       = False

instance Show Lisp where
  show l = case l of
    LList ls -> "(" ++ (concat $ intersperse " " $ map show ls) ++ ")"
    LAtom s -> s
    LNum n -> show n
    LTrue -> "#true"
    LFalse -> "#false"
    LNil -> "#nil"
    LCons a b -> "(cons " ++ show a ++ " " ++ show b ++ ")"
    LClosure as c e -> "(lam (" ++ (concat $ intersperse " " $ as) ++ ") Code"
    LError e -> "ERROR(" ++ e ++ ")"


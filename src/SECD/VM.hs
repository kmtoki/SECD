{-# LANGUAGE Strict, StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module SECD.VM (initVM,runVM) where

import SECD.Internal

import Prelude hiding (EQ)
import qualified Data.Map as M
import Unsafe.Coerce

initVM :: SECD
initVM = SECD [] M.empty [] []

runVM :: SECD -> IO SECD
runVM secd@SECD {..} = case code of
  [] -> pure secd
  DEBUG:ops -> do
    putStrLn $ "SECD:"
    putStrLn $ "stack: " ++ show stack
    putStrLn $ "code: " ++ show code 
    putStrLn $ "env: " ++ show env
    putStrLn $ "dump: " ++ show dump
    runVM $ secd { code = ops }
  PUTS:ops -> do
    print $ head stack
    runVM $ secd { stack = stack, code = ops }
  STOP:_ -> pure secd 
  op:ops -> runVM $ flip ($) secd { code = ops } $ case op of
    LET a   -> let' a
    LD a    -> ld a
    LDC l   -> ldc l
    LDF a c -> ldf a c
    LIST n  -> list n
    AP      -> ap
    RET     -> ret
    RAP     -> rap
    SEL a b -> sel a b
    JOIN    -> join
    CONS    -> cons
    CAR     -> car
    CDR     -> cdr
    EQ      -> eq
    ATOM    -> atom
    ADD     -> add

vmError :: String -> SECD -> SECD
vmError s secd@SECD {..} = 
  secd {
    stack = LError s : stack,
    code = STOP : code
  }

ld :: String -> SECD -> SECD
ld a secd@SECD {..} = 
  if M.member a env then
    secd {
      stack = (env M.! a) : stack
    }
  else
    vmError ("ld not found " ++ a) secd

ldc :: Lisp -> SECD -> SECD
ldc l secd@SECD {..} =
  secd {
    stack = l : stack
  }

ldf :: [String] -> Code -> SECD -> SECD
ldf as c secd@SECD {..} =
  secd {
    stack = LClosure as c env : stack
  }

list :: Int -> SECD -> SECD
list n secd@SECD {..} =
  secd {
    stack = LList (take n stack) : drop n stack
  }

ap :: SECD -> SECD
ap secd@SECD { stack = LClosure as c e : LList args : ss, ..} =
  secd {
    stack = [],
    env = M.union (M.fromList (zip as args)) e,
    code = c,
    dump = DumpAP ss env code : dump
  }
ap secd = vmError "ap error" secd

ret :: SECD -> SECD
ret secd@SECD {stack = s : ss, dump = DumpAP stack env code : dump } =
  secd {
    stack = s : stack,
    env = env,
    code = code,
    dump = dump
  }
ret secd = vmError "ret error" secd

rap :: SECD -> SECD
rap secd@SECD { stack = LClosure as c e : LList args : ss, ..} =
  secd {
    stack = [],
    env = M.union (M.union (M.fromList $ zip as args) e) env,
    code = c,
    dump = DumpAP ss env code : dump
  }

sel :: Code -> Code -> SECD -> SECD
sel t f secd@SECD {stack = s : ss, ..} = case s of
  LTrue  -> secd { code = t, dump = DumpSEL code : dump }
  LFalse -> secd { code = f, dump = DumpSEL code : dump }
  _      -> vmError ("sel error: expect bool. not " ++ show s) secd
sel t f secd@SECD { stack = [] } = 
  vmError ("vm error sel: expect bool. stack is empty") secd

join :: SECD -> SECD
join secd@SECD { dump = DumpSEL c : ds } =
  secd {
    code = c,
    dump = ds
  }

let' :: String -> SECD -> SECD
let' a secd@SECD { stack = s : ss } =
  secd {
    stack = ss,
    env = M.insert a s $ env secd
  }

cons :: SECD -> SECD
cons secd@SECD { stack =  a : b : ss } =
  secd {
    stack = LCons a b : ss
  }

car :: SECD -> SECD
car secd@SECD { stack = LCons a _ : ss } =
  secd {
    stack = a : ss
  }
car secd@SECD { stack = s : ss } =
  vmError ("car expect cons. not " ++ show s) secd

cdr :: SECD -> SECD
cdr secd@SECD { stack = LCons _ a : ss } = 
  secd { 
    stack = a : ss 
  }
cdr secd@SECD { stack = s : ss } =
  vmError ("cdr expect cons. not " ++ show s) secd

eq :: SECD -> SECD
eq secd@SECD { stack = a : b : ss } =
  secd {
    stack = (if a == b then LTrue else LFalse) : ss
  }

atom :: SECD -> SECD
atom secd@SECD { stack = a : ss } =
  secd {
    stack = s : ss
  }
  where
    s = case a of
      LClosure _ _ _ -> LFalse
      LList _ -> LFalse
      LCons _ _ -> LFalse
      LError _ -> LFalse
      _ -> LTrue

add :: SECD -> SECD
add secd@SECD { stack = LNum a : LNum b : ss } =
  secd {
    stack = LNum (a + b) : ss
  }


example = do
  secd <- runVM $ initVM { code = each }
  print secd
 where
  z = [
    LDF ["f"] [
      LDF ["x"] [
        LDF ["y"] [
          LD "y",
          LIST 1,
          LD "x",
          LIST 1,
          LD "x",
          AP,
          AP,
          RET
          ],
        LIST 1,
        LD "f",
        AP,
        RET
        ],
      LIST 1,
      LDF ["x"] [
        LDF ["y"] [
          LD "y",
          LIST 1,
          LD "x",
          LIST 1,
          LD "x",
          AP,
          AP,
          RET
          ],
        LIST 1,
        LD "f",
        AP,
        RET
        ],
      AP,
      RET
    ],
    LET "z"
    ]

  three = [
    LDF ["f"] [
      LDF ["x"] [
        LDC $ LNum 3,
        LD "x",
        EQ,
        SEL [LD "x", PUTS, JOIN] [
          LD "x",
          PUTS,
          LD "x",
          LDC $ LNum 1,
          ADD,
          LIST 1,
          LD "f",
          AP,
          JOIN
          ],
        RET
        ],
      RET
      ],
    LET "three"
    ]

  each = [
    LDF ["ls"] [
      LD "ls",
      ATOM,
      SEL [
        LD "ls",
        PUTS,
        JOIN
        ] [
        LD "ls",
        CAR,
        PUTS,
        LD "ls",
        CDR,
        LIST 1,
        LD "each",
        RAP,
        JOIN
        ],
      RET
      ],
    LET "each",
    LDC $ LNil,
    LDC $ LNum 1,
    LDC $ LNum 0,
    LIST 3,
    LIST 1,
    LD "each",
    RAP
    ]

  main' = z ++ three ++ [
    LDC $ LNum 0,
    LIST 1,
    LD "three",
    LIST 1,
    LD "z",
    AP,
    AP
    ]

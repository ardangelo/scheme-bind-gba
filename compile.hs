import Data.Int
import Data.Word
import Data.Bits
import Data.Monoid
import Data.Typeable
import Data.Binary.Put 
-- import Data.ByteString.Lazy.Char8

import Language.Scheme.Parser
import Language.Scheme.Types

import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.ByteString.Internal as BS (c2w, w2c)

import Control.Exception
import Control.Monad (when)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Except

import System.IO

-- Instructions and operands

data Op = IntOp Int32
        | RegOp Reg
        | AddrOp Word32
        | RegOffsOp Reg Int
  deriving (Eq, Show)

data Reg = R0 | R1 | R2 | LR | SP
  deriving (Eq, Show)

data Inst = BxInst Op
          | MovInst Op Op
          | LdrInst Op Op
          | AddInst Op Op
          | PushInst Op
          | PopInst Op
  deriving (Eq, Show)

-- State Machine

data Program = Program {
  _insts :: [String],
  _stack :: Int,
  _cleanregs :: Set Reg}
  deriving (Eq, Show)

type ARM a = State Program a

-- Modify State

emit :: String -> ARM ()
emit i = modify $ \s -> s {
  _insts = i:(_insts s)}
  
push :: Op -> ARM ()
push (RegOp rd) = modify $ \s -> s {
  _insts = ("str " ++ show rd ++ ", [sp, " ++ show (_stack s) ++ "]"):(_insts s),
  _stack = _stack s - 4}

pop :: Op -> ARM ()
pop (RegOp rd) = modify $ \s -> s {
  _insts = ("ldr " ++ show rd ++ ", [sp, " ++ show (_stack s + 4) ++ "]"):(_insts s),
  _stack = _stack s + 4}

-- Operands

r0 :: Op
r0 = RegOp R0

r1 :: Op
r1 = RegOp R1

r2 :: Op
r2 = RegOp R1

lr :: Op
lr = RegOp LR

sp :: Op
sp = RegOp SP

-- Instructions

bx :: Op -> ARM ()
bx (RegOp rd) = do
  emit $ "bx " ++ show rd

add :: Op -> Op -> Op -> ARM ()
add (RegOp rd) (RegOp rn) (RegOp op2) = do
  emit $ "add " ++ show rd ++ ", " ++ show rn ++ ", " ++ show op2
add (RegOp rd) (RegOp rn) (IntOp op2) = do
  emit $ "add " ++ show rd ++ ", " ++ show rn ++ ", #" ++ show op2

-- Pseudoinstructions

comment :: String -> ARM()
comment c = emit $ "@" ++ c

move :: Op -> Op -> ARM ()
move (RegOp rd) (RegOp op2) = do
  emit $ "mov " ++ show rd ++ ", " ++ show op2
move (RegOp rd) (IntOp op2) = do
  emit $ "ldr " ++ show rd ++ ", =" ++ show op2
move (RegOp rd) (RegOffsOp rn ro)
  | ro == 0 = emit $ "ldr " ++ show rd ++ ", [" ++ show rn ++ "]"
  | otherwise = emit $ "ldr " ++ show rd ++ ", [" ++ show rn ++ ", #" ++ show ro ++"]"
move (RegOffsOp rn ro) (RegOp rd)
  | ro == 0 = emit $ "str " ++ show rd ++ ", [" ++ show rn ++ "]"
  | otherwise = emit $ "str " ++ show rd ++ ", [" ++ show rn ++ ", #" ++ show ro ++"]"

-- Primitives

primitiveOps = H.fromList [
  ("+", do
      pop r0
      pop r1
      add r0 r0 r1)]

-- Compiler

compile :: LispVal -> ARM ()
compile (Number i) = move r0 (IntOp w)
  where w = fromInteger i :: Int32
compile (Atom a) =
  case H.lookup a primitiveOps of
    Just inst -> inst
    _ -> comment $ "atom '" ++ a ++ "' not found!"
compile (List l) =
  stackup $ reverse l
  -- for term in reverse tail l: compile l, push to stack, end for. pop into regs
  where stackup (e:es) = do
          compile e
          push r0
          stackup es
        stackup [] = pop r0

-- clean slate
start = Program { _insts = [], _stack = 0, _cleanregs = Set.empty }
-- create state transformations from source program
transforms str = case readExpr str of
  (Right e) -> do
    compile e
    bx lr
  _ -> comment "parse error"

-- REPL

prompt :: IO [String]
prompt = do
  putStr "> "
  hFlush stdout
  str <- getLine
  case str of
    ":q" -> pure []
    _ -> let final = execState (transforms str) start in
           pure $ reverse $ _insts final

main = do
  instrs <- prompt
  case instrs of
    [] -> pure ()
    _ -> do
      print instrs
      main

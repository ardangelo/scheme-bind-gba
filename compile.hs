-- Parser
import Language.Scheme.Parser
import Language.Scheme.Types

-- Data types
import Data.Int
import Data.Word

-- Machine state
import Control.Monad.Trans.State
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)
import Data.Set (Set)
import qualified Data.Set as Set

-- REPL
import System.IO

-- Instructions and operands

data Reg = R0 | R1 | R2 | LR | SP
  deriving (Eq, Show)

data Op = IntOp Int32
        | RegOp Reg
        | AddrOp Word32
        | RegOffsOp Reg Int
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
pop (RegOp rd) = modify $ \s ->
  case _stack s of
    0 -> s { _insts = "popped too much!":(_insts s) }
    _ -> s {
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
bx (RegOp rd) =
  emit $ "bx " ++ show rd

-- http://www.peter-cockerell.net/aalp/html/ch-3.html
-- <op1>{cond}{S}{P}  <dest>,<lhs>,<rhs>
g1o1 :: String -> Op -> Op -> Op -> ARM()
g1o1 inst (RegOp rd) (RegOp rn) boxedop2 =
  emit $ inst ++ " " ++ show rd ++ ", " ++ show rn ++ ", " ++ op2
  where op2 = case boxedop2 of
          (RegOp rop2) -> show rop2
          (IntOp iop2) -> "#" ++ show iop2
add = g1o1 "add"
adc = g1o1 "adc"
and = g1o1 "and"
bic = g1o1 "bic"
eor = g1o1 "eor"
orr = g1o1 "orr"
rsb = g1o1 "rsb"
rsc = g1o1 "rsc"
sbc = g1o1 "sbc"
sub = g1o1 "sub"

-- <op3>{cond}{S}{P}  <lhs>,<rhs>
g1o3 :: String -> Op -> Op -> ARM()
g1o3 inst (RegOp rn) boxedop2 =
  emit $ inst ++ " " ++ show rn ++ ", " ++ op2
  where op2 = case boxedop2 of
          (RegOp rop2) -> show rop2
          (IntOp iop2) -> "#" ++ show iop2
teq = g1o3 "teq"
tst = g1o3 "tst"
cmn = g1o3 "cmn"
cmp = g1o3 "cmp"

-- Pseudoinstructions

comment :: String -> ARM()
comment c = emit $ "@" ++ c

move :: Op -> Op -> ARM ()
move (RegOp rd) (RegOp op2) =
  emit $ "mov " ++ show rd ++ ", " ++ show op2
move (RegOp rd) (IntOp op2) =
  emit $ "ldr " ++ show rd ++ ", =" ++ show op2
move (RegOp rd) (RegOffsOp rn ro) =
  emit $ "ldr " ++ show rd ++ ", [" ++ show rn ++ ", #" ++ show ro ++ "]"
move (RegOffsOp rn ro) (RegOp rd) =
  emit $ "str " ++ show rd ++ ", [" ++ show rn ++ ", #" ++ show ro ++ "]"

-- Primitives

primitiveOps = H.fromList [
  ("add1", do
      pop r0
      add r0 r0 (IntOp 1)),
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
    _ -> emit $ "atom '" ++ a ++ "' not found!"
compile (List l) =
  stackup $ reverse l
  -- for term in reverse tail l: compile l, push to stack, end for. pop into regs
  where stackup (e:es) = do
          compile e
          push r0
          stackup es
        stackup [] = pop r0

-- initial state
start = Program { _insts = [], _stack = 0, _cleanregs = Set.empty }
-- create state transformations from source program
transforms str = case readExpr str of
  (Right e) -> do
    compile e
    bx lr
  _ -> emit $ "parse error: " ++ str

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

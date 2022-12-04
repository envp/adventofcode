module Main (main) where

import Control.Monad.State (execState, runState)
import qualified Control.Monad.State.Strict as StateM
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Char (isAlpha, isAlphaNum, isNumber)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as S
import Data.Tuple (swap)
import Data.Word (Word16)
import Debug.Trace
import GHC.Num (integerFromInt)
import qualified Parsing as P
import qualified Text.ParserCombinators.ReadP as R

newtype Symbol = Reference String deriving (Eq)

instance Ord Symbol where
  compare (Reference s1) (Reference s2) = compare s1 s2

instance Show Symbol where
  show (Reference s) = s

data Expr
  = Constant Word16
  | Variable Symbol
  | UnaryExpr UnaryOperator Symbol
  | BinaryExpr BinaryOperator Expr Expr
  deriving (Show)

newtype Statement = Assign (Expr, Symbol) deriving (Show)

data UnaryOperator
  = OpBitNot
  | OpLShift Int
  | OpRShift Int
  deriving (Show)

data BinaryOperator
  = OpBitAnd
  | OpBitOr
  deriving (Show)

data Operation
  = LoadConst Word16
  | Load Symbol
  | Store Symbol
  | ShiftL Int
  | ShiftR Int
  | And
  | Or
  | Not
  deriving (Show)

data VMError
  = UndefinedSymbol Symbol
  | StackUnderflow
  deriving (Show)

data VM = VM
  { vmSymbolTable :: M.Map Symbol Word16,
    vmStack :: [Word16],
    vmError :: Maybe VMError,
    vmIp :: Word16
  }
  deriving (Show)

type VMState = StateM.State VM ()

initVM :: VM
initVM = VM {vmSymbolTable = M.empty, vmStack = [], vmError = Nothing, vmIp = 0}

incrementIp :: VM -> VM
incrementIp vm@(VM {vmIp = ip}) = vm {vmIp = ip + 1}

showVmError :: VM -> String
showVmError VM {vmError = err, vmIp = ip} =
  "error: Failed to run instruction " ++ show ip ++ ": " ++ show err

getSymbolValue :: VM -> Symbol -> Maybe Word16
getSymbolValue vm key = M.lookup key (vmSymbolTable vm)

evaluate :: [Operation] -> VMState
evaluate [] = return ()
evaluate (op : rest) = do
  vm <- StateM.get
  let next =
        if isNothing (vmError vm)
          then process vm op
          else error (showVmError vm)
  StateM.put (incrementIp next)
  evaluate rest
  where
    process vm@(VM {vmStack = stack}) (LoadConst value) = vm {vmStack = value : stack}
    process vm@(VM {vmStack = stack}) (Load s) =
      case getSymbolValue vm s of
        Just value -> vm {vmStack = value : stack}
        _ -> vm {vmError = Just (UndefinedSymbol s)}
    process vm@(VM {vmSymbolTable = table, vmStack = stack}) (Store s) =
      case stack of
        (v : vs) -> vm {vmSymbolTable = M.insert s v table, vmStack = vs}
        _ -> vm {vmError = Just StackUnderflow}
    process vm@(VM {vmStack = stack}) (ShiftL n) =
      case stack of
        (v : vs) -> vm {vmStack = shiftL v n : vs}
        _ -> vm {vmError = Just StackUnderflow}
    process vm@(VM {vmStack = stack}) (ShiftR n) =
      case stack of
        (v : vs) -> vm {vmStack = shiftR v n : vs}
        _ -> vm {vmError = Just StackUnderflow}
    process vm@(VM {vmStack = stack}) And =
      case stack of
        (rhs : lhs : vs) -> vm {vmStack = (lhs .&. rhs) : vs}
        _ -> vm {vmError = Just StackUnderflow}
    process vm@(VM {vmStack = stack}) Or =
      case stack of
        (rhs : lhs : vs) -> vm {vmStack = (lhs .|. rhs) : vs}
        _ -> vm {vmError = Just StackUnderflow}
    process vm@(VM {vmStack = stack}) Not =
      case stack of
        (v : vs) -> vm {vmStack = complement v : vs}
        _ -> vm {vmError = Just StackUnderflow}

main :: IO ()
main = do
  input <- getContents
  let stmts = P.parse programP input
  let opcodes = compile stmts
  let vm = StateM.execState (evaluate opcodes) initVM
  let part1 = fromJust $ getSymbolValue vm (Reference "a")
  putStrLn $ "Part 1: " ++ show part1

--
-- Compiling Statements to Opcodes
--

compile :: [Statement] -> [Operation]
compile = concatMap transform . toposortStmts

transform :: Statement -> [Operation]
transform (Assign (expr, dest)) = transformExpr expr ++ [Store dest]
  where
    transformExpr :: Expr -> [Operation]
    transformExpr (Constant val) = [LoadConst val]
    transformExpr (Variable var) = [Load var]
    transformExpr (UnaryExpr op var) = [Load var, transformOp (Left op)]
    transformExpr (BinaryExpr op lhs rhs) = transformExpr lhs ++ [transformOp (Right op)] ++ transformExpr rhs

    transformOp :: Either UnaryOperator BinaryOperator -> Operation
    transformOp (Left OpBitNot) = Not
    transformOp (Left (OpLShift i)) = ShiftL i
    transformOp (Left (OpRShift i)) = ShiftR i
    transformOp (Right OpBitAnd) = And
    transformOp (Right OpBitOr) = Or

-- Reorder statements by their dependence on input wires
toposortStmts :: [Statement] -> [Statement]
toposortStmts stmts = undefined
  where
    invertMap :: (Ord a, Ord b) => M.Map a b -> M.Map b a
    invertMap = M.fromList . map swap . M.toList

    symbolVertex :: M.Map Symbol Int
    symbolVertex = invertMap vertexSymbol

    vertexSymbol :: M.Map Int Symbol
    vertexSymbol = M.fromAscList $ zip [0 ..] (S.elems $ S.fromList deps)
      where
        deps = concatMap (\(Assign (expr, dest)) -> findDependencies expr ++ [dest]) stmts

    toEdges :: Statement -> [(Symbol, Symbol)]
    toEdges (Assign (expr, dest)) = zip (findDependencies expr) (repeat dest)

    findDependencies :: Expr -> [Symbol]
    findDependencies Constant {} = []
    findDependencies (Variable symbol) = [symbol]
    findDependencies (UnaryExpr _ symbol) = [symbol]
    findDependencies (BinaryExpr _ lhs rhs) = concatMap findDependencies [lhs, rhs]

--
-- Parser
--

programP :: R.ReadP [Statement]
programP = do
  statements <- R.many1 statementP
  R.eof
  return statements

statementP :: R.ReadP Statement
statementP = do
  expr <- exprP
  R.skipSpaces
  _ <- R.string "->"
  R.skipSpaces
  symbol <- symbolP
  R.optional P.eol
  return $ Assign (expr, Reference symbol)

symbolP :: R.ReadP String
symbolP = do
  front <- R.munch1 isAlpha
  back <- R.munch isAlphaNum
  return $ front ++ back

exprP :: R.ReadP Expr
exprP =
  R.choice
    [ constantP,
      unaryExprP,
      binaryExprP
    ]
  where
    constantP :: R.ReadP Expr
    constantP = Constant . (\s -> fromInteger (integerFromInt s) :: Word16) <$> P.readInteger
    unaryExprP :: R.ReadP Expr
    unaryExprP =
      R.choice
        [ UnaryExpr OpBitNot <$> (R.string "NOT" >> R.skipSpaces >> (Reference <$> symbolP)),
          do
            symbol <- symbolP
            R.skipSpaces
            op <-
              R.choice
                [ R.string "LSHIFT" >> R.skipSpaces >> OpLShift <$> P.readInteger,
                  R.string "RSHIFT" >> R.skipSpaces >> OpRShift <$> P.readInteger
                ]
            return $ UnaryExpr op (Reference symbol),
          Variable . Reference <$> symbolP
        ]
    binaryExprP :: R.ReadP Expr
    binaryExprP = do
      lhs <- R.choice [constantP, Variable . Reference <$> symbolP]
      R.skipSpaces
      op <-
        R.choice
          [ R.string "AND" >> return OpBitAnd,
            R.string "OR" >> return OpBitOr
          ]
      R.skipSpaces
      rhs <- R.choice [constantP, Variable . Reference <$> symbolP]
      return $ BinaryExpr op lhs rhs

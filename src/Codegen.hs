{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}

module Codegen where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map           as Map
import           Data.Maybe         (fromJust)
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8)

import qualified LLVM.AST                   as AST
import qualified LLVM.AST.Constant          as Const
import qualified LLVM.AST.IntegerPredicate  as P
import qualified LLVM.AST.Type              as Ty
import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Module      as IR
import qualified LLVM.IRBuilder.Monad       as IR

import qualified LLVM.Context as LLVM
import qualified LLVM.Module  as LLVM

import qualified Errors    as Err
import           Hoist
import qualified Operators as Op
import           Utils

-- TODO: every function takes two parameters, fix it with typing

genericPtr :: Ty.Type
genericPtr = Ty.ptr Ty.i8

functionType :: Ty.Type
functionType = Ty.FunctionType genericPtr [genericPtr, genericPtr] False

constInt :: Int -> AST.Operand
constInt i = AST.ConstantOperand $ Const.Int 64 $ toInteger i

namedFunction :: Ty.Type -> String -> AST.Operand
namedFunction t = AST.ConstantOperand . Const.GlobalReference (Ty.ptr t) . AST.mkName

callMalloc :: IR.MonadIRBuilder m => AST.Operand -> m AST.Operand
callMalloc len = IR.call malloc [(len, [])]
  where
    malloc = namedFunction (Ty.FunctionType genericPtr [Ty.i64] False) "malloc"

callMalloc' :: IR.MonadIRBuilder m => AST.Operand -> m AST.Operand
callMalloc' len = do
  m <- callMalloc len
  IR.bitcast m $ Ty.ptr genericPtr

data Env =
  Env { localBound :: Maybe AST.Operand
      , args       :: [AST.Operand]
      , letBound   :: [AST.Operand] }
withLocalBound :: MonadReader Env m => AST.Operand -> m AST.Operand -> m AST.Operand
withLocalBound newBound = local update
  where
    update x = x { localBound = Just newBound }
getLocalBound :: MonadReader Env m => m AST.Operand
getLocalBound = asks $ fromJust . localBound

withLetBound :: MonadReader Env m => AST.Operand -> m AST.Operand -> m AST.Operand
withLetBound bs = local update
  where
    update x = x { letBound = bs : letBound x }
getLetBound :: MonadReader Env m => Int -> m AST.Operand
getLetBound idx = asks $ (!! idx) . letBound

initEnv :: Env
initEnv = Env Nothing [] []
initArg :: [AST.Operand] -> Env
initArg xs = initEnv { args = xs }

type NameMap = Map.Map String AST.Operand

initNameMap :: NameMap
initNameMap = Map.empty

genExpr :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m, MonadFix m, MonadReader Env m, MonadState NameMap m) => Expr -> m AST.Operand
genExpr (Integer i) = IR.inttoptr (constInt i) genericPtr
genExpr (Parameter i) = asks $ (!! i) . args
genExpr (NameRef name) = gets (Map.! name)
genExpr (FunctionRef i) = IR.bitcast (namedFunction functionType $ nameFunction i) genericPtr
genExpr (LetRef i) = getLetBound i
genExpr (Call f a) = do
  f' <- genExpr f
  f' <- IR.bitcast f' $ Ty.ptr functionType
  a' <- mapM genExpr a
  IR.call f' $ map (,[]) $ pad a'
  where
    pad xs = take 2 $ xs ++ repeat (AST.ConstantOperand $ Const.Null genericPtr)
genExpr (Tuple xs) = do
  xs' <- mapM genExpr xs
  m <- callMalloc' $ constInt $ length xs * 8
  forM_ (zip [0..] xs') $ \(i, x) -> do
    e <- IR.gep m [constInt i]
    IR.store e 0 x
  IR.bitcast m genericPtr
genExpr (NthOf i e) = do
  e' <- genExpr e
  e' <- IR.bitcast e' $ Ty.ptr genericPtr
  ptr <- IR.gep e' [constInt i]
  IR.load ptr 0
genExpr (LocalLet e x) = do
  e' <- genExpr e
  withLocalBound e' $ genExpr x
genExpr LocalBound = getLocalBound
genExpr (LetIn def body) = do
  def' <- genExpr def
  withLetBound def' $ genExpr body
genExpr (BinaryOp op l r) = join $ apply_op <$> genExpr l <*> genExpr r
  where
    apply_op a b = do
      a' <- IR.ptrtoint a Ty.i64
      b' <- IR.ptrtoint b Ty.i64
      x <- opr a' b'
      IR.inttoptr x genericPtr
    opr =
      case op of
        Op.Add -> IR.add
        Op.Mul -> IR.mul
        Op.Sub -> IR.sub
        Op.Eq -> \a b -> do
          x <- IR.icmp P.EQ a b
          IR.zext x Ty.i64

genExpr (SingleOp op e) = apply_op =<< genExpr e
  where
    apply_op v = do
      v' <- IR.ptrtoint v Ty.i64
      x <- opr v'
      IR.inttoptr x genericPtr
    opr =
      case op of
        Op.Negative -> IR.sub $ constInt 0
        Op.Positive -> return

genExpr Alloc = callMalloc $ constInt 8
genExpr (Ref e) = do
  e' <- genExpr e
  m <- callMalloc' $ constInt 8
  IR.store m 0 e'
  IR.bitcast m genericPtr

genExpr (Seq l r) = genExpr l >> genExpr r

genExpr (Assign l r) = do
  l' <- genExpr l
  r' <- genExpr r
  dest <- IR.bitcast l' $ Ty.ptr genericPtr
  IR.store dest 0 r'
  return r'

genExpr (Deref e) = do
  e' <- genExpr e
  ptr <- IR.bitcast e' $ Ty.ptr genericPtr
  IR.load ptr 0

genExpr (If c t e) = mdo
  c' <- flip IR.ptrtoint Ty.i64 =<< genExpr c
  res <- IR.alloca genericPtr Nothing 4
  cond <- IR.icmp P.EQ c' $ constInt 0
  IR.condBr cond ifElse ifThen
  ifThen <- IR.block
  t' <- genExpr t
  IR.store res 0 t'
  IR.br ifExit
  ifElse <- IR.block
  e' <- genExpr e
  IR.store res 0 e'
  IR.br ifExit
  ifExit <- IR.block
  IR.load res 0

genExpr (Error err) = do
  let puts = namedFunction (Ty.FunctionType Ty.i32 [Ty.ptr Ty.i8] False) "puts"
  msg <- IR.globalStringPtr (Err.message err) "err"
  _ <- IR.call puts [(msg, [])]
  let exit = namedFunction (Ty.FunctionType Ty.void [Ty.i32] False) "exit"
  _ <- IR.call exit [(AST.ConstantOperand $ Const.Int 32 1, [])]
  IR.inttoptr (constInt 0) genericPtr

genFunction :: (IR.MonadModuleBuilder m, MonadFix m) => String -> Function -> m AST.Operand
genFunction name (Function n expr) =
  IR.function (AST.mkName name) params genericPtr $ \argList ->
    -- prevent the use of NameRef by passing empty NameMap as a initial state
    IR.ret =<< evalStateT (runReaderT (genExpr expr) (initArg argList)) initNameMap
  where
    params = replicate n (genericPtr, IR.NoParameterName)

genTopExpr :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m, MonadFix m, MonadState NameMap m) => Expr -> m AST.Operand
genTopExpr e = runReaderT (genExpr e) initEnv

genDef :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m, MonadFix m, MonadState NameMap m) => Def -> m ()
genDef (Name name body) = do
  e <- genTopExpr body
  modify $ Map.insert name e

genCode :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m, MonadFix m) => Code -> m AST.Operand
genCode (Code defs entry) = evalStateT gen initNameMap
  where
    gen = mapM_ genDef defs >> genTopExpr entry

nameFunction :: Int -> String
nameFunction i = "__faber_fn_" ++ show i

codegen :: Module -> AST.Module
codegen m = IR.buildModule "faber-output" $ do
  _ <- IR.extern "malloc" [Ty.i64] genericPtr
  _ <- IR.extern "puts" [Ty.ptr Ty.i8] Ty.i32
  _ <- IR.extern "exit" [Ty.i32] Ty.void
  zipWithM_ (genFunction . nameFunction) [0..] (functions m)
  IR.function "main" [(Ty.i32, "argc"), (Ty.ptr (Ty.ptr Ty.i8), "argv")] Ty.i32 $ \[_, _] -> do
    ret <- genCode $ code m
    int <- IR.ptrtoint ret Ty.i64
    printf <- IR.externVarArgs "printf" [Ty.ptr Ty.i8] Ty.i32
    fmt <- IR.globalStringPtr "%d\n" "fmt"
    _ <- IR.call printf [(fmt, []), (int, [])]
    IR.ret $ AST.ConstantOperand $ Const.Int 32 0

toLLVM :: AST.Module -> IO Text
toLLVM m = LLVM.withContext $ \ctx ->
  decodeUtf8 <$> LLVM.withModuleFromAST ctx m LLVM.moduleLLVMAssembly

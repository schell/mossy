{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mossy.Emit where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State
import           Data.String            (fromString)
--import qualified LLVM.AST                  as AST
--import qualified LLVM.AST.Constant         as C
--import qualified LLVM.AST.IntegerPredicate as IP
--import qualified LLVM.AST.Type             as T
import           Language.GLSL.Syntax   as AST

--import           Mossy.Codegen
import           Mossy.JIT
import           Mossy.Eval
import           Mossy.Pretty           (pcShow)
import qualified Mossy.Syntax           as S

--------------------------------------------------------------------------------
-- Basic Expressions
--------------------------------------------------------------------------------
constantTrue :: Expr
constantTrue  = BoolConstant True

constantFalse :: Expr
constantFalse = BoolConstant False

constantZero :: Expr
constantZero  = IntConstant Decimal 0

constantOne :: Expr
constantOne  = IntConstant Decimal 1

int :: Int -> Expr
int = IntConstant Decimal . toInteger

bool :: Bool -> Expr
bool = BoolConstant

var :: String -> Expr
var = Variable

--------------------------------------------------------------------------------
-- Building the code generator
--------------------------------------------------------------------------------
data CodegenState a = CS { csDecls :: [a]
                         , csNextK :: Word
                         }

emptyEx :: CodegenState a
emptyEx = CS [] 0

type Codegen a  = StateT (CodegenState a)
type CodegenEx  = Codegen ExternalDeclaration
type CodegenMid = Codegen Statement

data GLSLType = GLSLType { glslType       :: TypeSpecifierNonArray
                         , glslArrayStuff :: Maybe (Maybe Expr)
                         } deriving (Show, Eq)

toGLSLTypeSpec :: GLSLType -> TypeSpecifier
toGLSLTypeSpec (GLSLType retty rettyarr) =
  TypeSpec Nothing $ TypeSpecNoPrecision retty rettyarr

decl :: Monad m => a -> Codegen a m ()
decl decl = modify $ \s -> s{ csDecls = csDecls s ++ [decl] }

initialized :: Monad m => GLSLType -> String -> Maybe Expr -> CodegenEx m Expr
initialized glslTyp name mayExpr = do
  let typespec = toGLSLTypeSpec glslTyp
      initDecl = InitDecl name Nothing mayExpr
  decl $ Declaration $ InitDeclaration (TypeDeclarator $ FullType Nothing typespec) [initDecl]
  return $ Variable name

uninitialized :: Monad m => GLSLType -> String -> CodegenEx m Expr
uninitialized glslTyp name = initialized glslTyp name Nothing

fresh :: Monad m => Codegen a m Word
fresh = do
  CS decls k <- get
  put $ CS decls $ succ k
  return k

runNewEx :: Monad m => Codegen a m b -> m (b, CodegenState a)
runNewEx m = runStateT m emptyEx

cgen :: S.Expr -> CodegenEx IO Expr
cgen expr = case fst $ runEval expr of
  VInt i -> return $ int i
  VBool b -> return $ bool b
  VClosure param body env -> undefined


--codegenTop :: S.Expr -> LLVM ()
--codegenTop expr = define T.void (fromString "main") [] blks
--  -- we currently don't have any top level declarations in mossy
--  where blks = createBlocks $ execCodegen $ do
--          ent <- addBlock entryBlockName
--          _   <- setBlock ent
--          void $ cgen expr
--          retVoid

voidType :: GLSLType
voidType = GLSLType Void Nothing

--codegenMain :: Codegen Expr -> GLSL ()
--codegenMain code = define voidType "main" [] blks
--  where blks = createBlocks $ execCodegen $ do
--          ent <- addBlock entryBlockName
--          _   <- setBlock ent
--          a   <- code
--          ret $ Just a

---------------------------------------------------------------------------------
---- Compilation
---------------------------------------------------------------------------------
--codegen :: AST.Module -> [S.Expr] -> IO AST.Module
--codegen mdl fns = runJIT oldast
--  where modn   = mapM codegenTop fns
--        oldast = runLLVM mdl modn
codegen :: S.Expr -> IO TranslationUnit
codegen expr = do
  (outExpr, cs) <- runNewEx $ cgen expr
  print outExpr
  return $ TranslationUnit $ csDecls cs

--test = putStrLn $ pcShow $ TranslationUnit $ runGLSL [] $ codegenMain testComp
--
--testComp :: Codegen ()
--testComp = undefined

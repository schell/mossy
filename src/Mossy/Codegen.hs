{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Mossy.Codegen where

import           Control.Arrow       ((&&&))
import           Control.Monad.State (MonadState (..), State, execState, runState, gets,
                                      modify)
import           Data.Function       (on)
import           Data.List           (sortBy)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (maybeToList)
import           Data.Semigroup      ((<>))
import           Data.String         (fromString)
import           Language.GLSL.Syntax       as AST
--import           LLVM.AST                        as AST
--import qualified LLVM.Attribute              as A
--import qualified LLVM.CallingConvention      as CC
--import qualified LLVM.Constant               as C
--import qualified LLVM.FloatingPointPredicate as FP
--import           LLVM.Global
--import qualified LLVM.IntegerPredicate       as IP
--import qualified LLVM.Linkage                as L
--import qualified LLVM.Type                   as T
--import           LLVM.Prelude

--------------------------------------------------------------------------------
-- Code gen setup
--------------------------------------------------------------------------------
type Names = Map String Int

data Name = Name String | UnName Word deriving (Show, Eq, Ord)

data Named a = Name := a | UnNamed a deriving (Show, Eq)

fromNamed :: Named a -> a
fromNamed (_ := a)    = a
fromNamed (UnNamed a) = a

type SymbolTable = [(String, Expr)]

data CodegenState = CodegenState { codegenCurrentBlock :: Name
                                 -- ^ Name of the active block to append to
                                 , codegenBlocks       :: Map Name BlockState
                                 -- ^ Blocks for functions
                                 , codegenSymtab       :: SymbolTable
                                 -- ^ Function scope symbol table
                                 , codegenBlockCount   :: Int
                                 -- ^ Count of basic blocks
                                 , codegenCount        :: Word
                                 -- ^ Count of unnamed instructions
                                 , codegenNames        :: Names
                                 -- ^ Name supply
                                 } deriving (Show)

data BlockState = BlockState { blockStateIdx   :: Int
                             -- ^ Block index
                             , blockStateStack :: [Named Statement]
                             -- ^ Stack of instructions
                             , blockStateTerminator :: Maybe (Named Statement)
                             } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) M.empty [] 1 0 M.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

--newtype LLVM a = LLVM (State Module a)
--  deriving (Functor, Applicative, Monad, MonadState Module)

newtype GLSL a = GLSL (State [ExternalDeclaration] a)
  deriving (Functor, Applicative, Monad, MonadState [ExternalDeclaration])

--runLLVM :: Module -> LLVM a -> Module
--runLLVM mdl (LLVM m) = execState m mdl

runGLSL :: [ExternalDeclaration] -> GLSL a -> [ExternalDeclaration]
runGLSL mdl (GLSL m) = execState m mdl

--emptyModule :: String -> Module
--emptyModule label = defaultModule { moduleName = label }

emptyModule :: String -> [ExternalDeclaration]
emptyModule label = []

--addDefn :: Definition -> LLVM ()
--addDefn d = do
--  defs <- gets moduleDefinitions
--  modify $ \s -> s{ moduleDefinitions = defs ++ [d] }

addDefn :: ExternalDeclaration -> GLSL ()
addDefn d = do
  decls <- get
  put $ decls ++ [d]

--functionDef :: Type -> String -> [(Type, Name)] -> Global
--functionDef retty label argtys =
--  functionDefaults { name        = Name label
--                   , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
--                   , returnType  = retty
--                   }

data GLSLType = GLSLType { glslType       :: TypeSpecifierNonArray
                         , glslArrayStuff :: Maybe (Maybe Expr)
                         } deriving (Show, Eq)

toParamName :: Name -> String
toParamName = \case
  Name n   -> n
  UnName i -> "p" ++ show i

toGLSLParam :: (GLSLType, Name) -> ParameterDeclaration
toGLSLParam (typ, name) = ParameterDeclaration Nothing
                                               Nothing
                                               (toGLSLTypeSpec typ)
                                               (Just (toParamName name, Nothing))


toGLSLTypeSpec :: GLSLType -> TypeSpecifier
toGLSLTypeSpec (GLSLType retty rettyarr) =
  TypeSpec Nothing $ TypeSpecNoPrecision retty rettyarr

functionProto
  :: GLSLType
  -> String
  -> [(GLSLType, Name)]
  -> FunctionPrototype
functionProto (GLSLType retty rettyarr) label =
  FuncProt (FullType Nothing typspec) label . map toGLSLParam
  where typspec = TypeSpec Nothing $ TypeSpecNoPrecision retty rettyarr

--define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
--define retty label argtys body = addDefn $ GlobalDefinition $
--  (functionDef retty label argtys) { basicBlocks = body }

blockToStatement :: GLSLBlock -> [Statement]
blockToStatement (GLSLBlock _ ss t) = ss ++ [fromNamed t]

define :: GLSLType -> String -> [(GLSLType, Name)] -> [GLSLBlock] -> GLSL ()
define retty label argtys body = do
  let proto = functionProto retty label argtys
  addDefn $ FunctionDefinition proto $ Compound $ concatMap blockToStatement body

--external :: Type -> String -> [(Type, Name)] -> LLVM ()
--external retty label argtys = addDefn $ GlobalDefinition $
--  (functionDef retty label argtys) { linkage = L.External }

--------------------------------------------------------------------------------
-- Blocks
--------------------------------------------------------------------------------
data GLSLBlock = GLSLBlock Name [Statement] (Named Statement)

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

entryBlockName :: String
entryBlockName = fromString "entry"

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns
  | Just ix <- M.lookup nm ns = (nm <> fromString (show ix), M.insert nm (succ ix) ns)
  | otherwise                 = (nm, M.insert nm 1 ns)

entry :: Codegen Name
entry = gets codegenCurrentBlock

-- Why reverse s?
--makeBlock :: (Name, BlockState) -> BasicBlock
--makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
--  where
--    maketerm (Just x) = x
--    maketerm Nothing  = error $ "Block has no terminator: " ++ (show l)

makeBlock :: (Name, BlockState) -> GLSLBlock
makeBlock (l, BlockState _ s t) =
  GLSLBlock l (reverse $ map fromNamed s) (maketerm t)
  where maketerm (Just x) = x
        maketerm Nothing  = UnNamed $ Return Nothing



sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (blockStateIdx . snd))

--createBlocks :: CodegenState -> [BasicBlock]
--createBlocks = map makeBlock . sortBlocks . M.toList . blocks

createBlocks :: CodegenState -> [GLSLBlock]
createBlocks = map makeBlock . sortBlocks . M.toList . codegenBlocks

addBlock :: String -> Codegen Name
addBlock bname = do
  (bls, (ix, nms)) <- gets (codegenBlocks &&& codegenBlockCount &&& codegenNames)

  let new             = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { codegenBlocks = M.insert (Name qname) new bls
                   , codegenBlockCount = succ ix
                   , codegenNames = supply
                   }
  return $ Name qname

setBlock :: Name -> Codegen ()
setBlock bname = modify $ \s -> s { codegenCurrentBlock = bname }

getBlock :: Codegen Name
getBlock = gets codegenCurrentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  (active, blocks) <- gets (codegenCurrentBlock &&& codegenBlocks)
  modify $ \s -> s { codegenBlocks = M.insert active new blocks }

current :: Codegen BlockState
current = do
  c <- gets codegenCurrentBlock
  blks <- gets codegenBlocks
  case M.lookup c blks of
    Just x  -> return x
    Nothing -> error $ "No such block: " ++ show c

--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------
fresh :: Codegen Word
fresh = do
  k <- gets codegenCount
  modify $ \s -> s{ codegenCount = succ k }
  return k

--local :: Name -> Expr
--local = LocalReference T.double

--externf :: Name -> Expr
--externf = ConstantExpr . C.GlobalReference T.double

assign :: String -> Expr -> Codegen ()
assign var x = do
  lcls <- gets codegenSymtab
  modify $ \s -> s { codegenSymtab = (var, x) : lcls }

getvar :: String -> Codegen Expr
getvar var = do
  syms <- gets codegenSymtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

statement :: Statement -> Codegen ()
statement ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = blockStateStack blk
  modifyBlock $ blk{ blockStateStack = (ref := ins) : i }

--terminator :: Named Terminator -> Codegen (Named Terminator)
--terminator trm = do
--  blk <- current
--  modifyBlock $ blk{ term = Just trm }
--  return trm

ret :: Maybe Expr -> Codegen ()
ret = statement . Return
--------------------------------------------------------------------------------
-- Arithmetic operations
--------------------------------------------------------------------------------
--fadd :: Expr -> Expr -> Codegen Expr
--fadd a b =  $ FAdd NoFastMathFlags a b []
--
--fsub :: Expr -> Expr -> Codegen Expr
--fsub a b = instr $ FSub NoFastMathFlags a b []
--
--fmul :: Expr -> Expr -> Codegen Expr
--fmul a b = instr $ FMul NoFastMathFlags a b []
--
--fdiv :: Expr -> Expr -> Codegen Expr
--fdiv a b = instr $ FDiv NoFastMathFlags a b []
--
--fcmp :: FP.FloatingPointPredicate -> Expr -> Expr -> Codegen Expr
--fcmp cond a b = instr $ FCmp cond a b []
--
--iadd :: Expr -> Expr -> Codegen Expr
--iadd a b = instr $ Add True True a b []
--
--isub :: Expr -> Expr -> Codegen Expr
--isub a b = instr $ Sub True True a b []
--
--icmp :: IP.IntegerPredicate -> Expr -> Expr -> Codegen Expr
--icmp cond a b = instr $ ICmp cond a b []
--
--br :: Name -> Codegen (Named Terminator)
--br val = terminator $ Do $ Br val []
--
--cbr :: Expr -> Name -> Name -> Codegen (Named Terminator)
--cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []
--
--phi :: Type -> [(Expr, Name)] -> Codegen Expr
--phi ty incoming = instr $ Phi ty incoming []
--
--ret :: Expr -> Codegen (Named Terminator)
--ret val = terminator $ Do $ Ret (Just val) []
--
--retVoid :: Codegen (Named Terminator)
--retVoid = terminator $ Do $ Ret Nothing []
--
--toArgs :: [Expr] -> [(Expr, [A.ParameterAttribute])]
--toArgs = map (\x -> (x, []))
--
--constant :: C.Constant -> Expr
--constant = ConstantExpr

-- | Take a 'Named' function reference anda  list of arguments and evaluate it,
-- invoke the function with the args at the current position, returning an
-- operand.
--call :: Expr -> [Expr] -> Codegen Expr
--call fn rgs = instr $ Call Nothing CC.C [] (Right fn) (toArgs rgs) [] []

-- | Create a pointer to a stack allocated, uninitiallized value of the given
-- type.
--alloca :: Type -> Codegen Expr
--alloca ty = instr $ Alloca ty Nothing 0 []

-- | Store a value at an address.
--store :: Expr -> Expr -> Codegen Expr
--store ptr val = instr $ Store False ptr val Nothing 0 []

-- | Load a value from an address.
--load :: Expr -> Codegen Expr
--load ptr = instr $ Load False ptr Nothing 0 []

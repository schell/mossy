{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Mossy.Codegen where

import           Control.Monad.State (MonadState (..), State, execState, gets,
                                      modify)
import           Data.Function       (on)
import           Data.List           (sortBy)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (maybeToList)
import           Data.String         (fromString)
import           Language.GLSL       as AST
--import           LLVM.AST                        as AST
--import qualified LLVM.AST.Attribute              as A
--import qualified LLVM.AST.CallingConvention      as CC
--import qualified LLVM.AST.Constant               as C
--import qualified LLVM.AST.FloatingPointPredicate as FP
--import           LLVM.AST.Global
--import qualified LLVM.AST.IntegerPredicate       as IP
--import qualified LLVM.AST.Linkage                as L
--import qualified LLVM.AST.Type                   as T
--import           LLVM.Prelude

--------------------------------------------------------------------------------
-- Code gen setup
--------------------------------------------------------------------------------
type Names = Map String Int

data Name = Name String | UnName Int deriving (Show, Eq)

data Named a = Name := a | UnNamed a deriving (Show, Eq)

data Operand = Operand { operandName :: Name } deriving (Show, Eq)

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState { currentBlock :: Name
                                 -- ^ Name of the active block to append to
                                 , blocks       :: Map Name BlockState
                                 -- ^ Blocks for functions
                                 , symtab       :: SymbolTable
                                 -- ^ Function scope symbol table
                                 , blockCount   :: Int
                                 -- ^ Count of basic blocks
                                 , count        :: Word
                                 -- ^ Count of unnamed instructions
                                 , names        :: Names
                                 -- ^ Name supply
                                 } deriving (Show)

data BlockState = BlockState { idx   :: Int
                             -- ^ Block index
                             , stack :: [Named Statement]
                             -- ^ Stack of instructions
                             , term  :: Maybe (Named Statement)
                             -- ^ Block terminator
                             } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) M.empty [] 1 0 M.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

--newtype LLVM a = LLVM (State AST.Module a)
--  deriving (Functor, Applicative, Monad, MonadState AST.Module)

newtype GLSL a = GLSL (State [AST.ExternalDeclaration] a)
  deriving (Functor, Applicative, Monad, MonadState [AST.ExternalDeclaration])

--runLLVM :: AST.Module -> LLVM a -> AST.Module
--runLLVM mdl (LLVM m) = execState m mdl

runGLSL :: [AST.ExternalDeclaration] -> GLSL a -> [AST.ExternalDeclaration]
runGLSL mdl (GLSL m) = execState m mdl

--emptyModule :: String -> AST.Module
--emptyModule label = defaultModule { moduleName = label }

emptyModule :: String -> [AST.ExternalDeclaration]
emptyModule label = []

--addDefn :: Definition -> LLVM ()
--addDefn d = do
--  defs <- gets moduleDefinitions
--  modify $ \s -> s{ moduleDefinitions = defs ++ [d] }

addDefn :: AST.ExternalDeclaration -> GLSL ()
addDefn d = do
  decls <- get
  put $ decls ++ [d]

--functionDef :: Type -> String -> [(Type, Name)] -> Global
--functionDef retty label argtys =
--  functionDefaults { name        = Name label
--                   , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
--                   , returnType  = retty
--                   }

data GLSLType = GLSLType { glslType       :: AST.TypeSpecifierNonArray
                         , glslArrayStuff :: Maybe (Maybe AST.Expr)
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
  -> AST.FunctionPrototype
functionProto (GLSLType retty rettyarr) label =
  FuncProt (FullType Nothing typspec) label . map toGLSLParam
  where typspec = TypeSpec Nothing $ TypeSpecNoPrecision retty rettyarr

--define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
--define retty label argtys body = addDefn $ GlobalDefinition $
--  (functionDef retty label argtys) { basicBlocks = body }

blockToStatement :: GLSLBlock -> [Statement]
blockToStatement (GLSLBlock _ ss mt) = ss ++ maybeToList mt

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
entry = gets currentBlock

-- Why reverse s?
--makeBlock :: (Name, BlockState) -> BasicBlock
--makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
--  where
--    maketerm (Just x) = x
--    maketerm Nothing  = error $ "Block has no terminator: " ++ (show l)

makeBlock :: (Name, BlockState) -> GLSLBlock
makeBlock (l, (BlockState _ s t)) = GLSLBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing  = error $ "Block has no terminator: " ++ (show l)



sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

--createBlocks :: CodegenState -> [BasicBlock]
--createBlocks = map makeBlock . sortBlocks . M.toList . blocks

createBlocks :: CodegenState -> [GLSLBlock]
createBlocks = map makeBlock . sortBlocks . M.toList . blocks

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new             = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = M.insert (Name qname) new bls
                   , blockCount = succ ix
                   , names = supply
                   }
  return $ Name qname

setBlock :: Name -> Codegen ()
setBlock bname = modify $ \s -> s { currentBlock = bname }

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = M.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case M.lookup c blks of
    Just x  -> return x
    Nothing -> error $ "No such block: " ++ show c

--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------
fresh :: Codegen Word
fresh = do
  k <- gets count
  modify $ \s -> s{ count = succ k }
  return k

--local :: Name -> Operand
--local = LocalReference T.double

--externf :: Name -> Operand
--externf = ConstantOperand . C.GlobalReference T.double

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = (var, x) : lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

instr :: Statement -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock $ blk{ stack = (ref := ins) : i }
  return $ Operand ref

--terminator :: Named Terminator -> Codegen (Named Terminator)
--terminator trm = do
--  blk <- current
--  modifyBlock $ blk{ term = Just trm }
--  return trm

terminator :: Named Statement -> Codegen ()
terminator trm = do
  blk <- current
  modifyBlock $ blk{ term = Just trm }
  return trm
--------------------------------------------------------------------------------
-- Arithmetic operations
--------------------------------------------------------------------------------
--fadd :: Operand -> Operand -> Codegen Operand
--fadd a b = instr $ FAdd NoFastMathFlags a b []
--
--fsub :: Operand -> Operand -> Codegen Operand
--fsub a b = instr $ FSub NoFastMathFlags a b []
--
--fmul :: Operand -> Operand -> Codegen Operand
--fmul a b = instr $ FMul NoFastMathFlags a b []
--
--fdiv :: Operand -> Operand -> Codegen Operand
--fdiv a b = instr $ FDiv NoFastMathFlags a b []
--
--fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
--fcmp cond a b = instr $ FCmp cond a b []
--
--iadd :: Operand -> Operand -> Codegen Operand
--iadd a b = instr $ Add True True a b []
--
--isub :: Operand -> Operand -> Codegen Operand
--isub a b = instr $ Sub True True a b []
--
--icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
--icmp cond a b = instr $ ICmp cond a b []
--
--br :: Name -> Codegen (Named Terminator)
--br val = terminator $ Do $ Br val []
--
--cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
--cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []
--
--phi :: Type -> [(Operand, Name)] -> Codegen Operand
--phi ty incoming = instr $ Phi ty incoming []
--
--ret :: Operand -> Codegen (Named Terminator)
--ret val = terminator $ Do $ Ret (Just val) []
--
--retVoid :: Codegen (Named Terminator)
--retVoid = terminator $ Do $ Ret Nothing []
--
--toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
--toArgs = map (\x -> (x, []))
--
--constant :: C.Constant -> Operand
--constant = ConstantOperand

-- | Take a 'Named' function reference anda  list of arguments and evaluate it,
-- invoke the function with the args at the current position, returning an
-- operand.
--call :: Operand -> [Operand] -> Codegen Operand
--call fn rgs = instr $ Call Nothing CC.C [] (Right fn) (toArgs rgs) [] []

-- | Create a pointer to a stack allocated, uninitiallized value of the given
-- type.
--alloca :: Type -> Codegen Operand
--alloca ty = instr $ Alloca ty Nothing 0 []

-- | Store a value at an address.
--store :: Operand -> Operand -> Codegen Operand
--store ptr val = instr $ Store False ptr val Nothing 0 []

-- | Load a value from an address.
--load :: Operand -> Codegen Operand
--load ptr = instr $ Load False ptr Nothing 0 []

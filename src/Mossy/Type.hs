module Mossy.Type
  ( MossyType(..)
  , Type(..)
  ) where

data Type = TBool | TInt | TArr Type Type deriving (Show, Eq)

data MossyType = MossyFloat
               | MossyInt
               | MossyUInt
               | MossyBool
               | MossyVec2
               | MossyVec3
               | MossyVec4
               | MossyBVec2
               | MossyBVec3
               | MossyBVec4
               | MossyIVec2
               | MossyIVec3
               | MossyIVec4
               | MossyUVec2
               | MossyUVec3
               | MossyUVec4
               | MossyMat2
               | MossyMat3
               | MossyMat4
               | MossyMat2x2
               | MossyMat2x3
               | MossyMat2x4
               | MossyMat3x2
               | MossyMat3x3
               | MossyMat3x4
               | MossyMat4x2
               | MossyMat4x3
               | MossyMat4x4
               | MossySampler1D
               | MossySampler2D
               | MossySampler3D
--  , SamplerCube
--  , Sampler1DShadow
--  , Sampler2DShadow
--  , SamplerCubeShadow
--  , Sampler1DArray
--  , Sampler2DArray
--  , Sampler1DArrayShadow
--  , Sampler2DArrayShadow
--  , ISampler1D
--  , ISampler2D
--  , ISampler3D
--  , ISamplerCube
--  , ISampler1DArray
--  , ISampler2DArray
--  , USampler1D
--  , USampler2D
--  , USampler3D
--  , USamplerCube
--  , USampler1DArray
--  , USampler2DArray
--  , Sampler2DRect
--  , Sampler2DRectShadow
--  , ISampler2DRect
--  , USampler2DRect
--  , SamplerBuffer
--  , ISamplerBuffer
--  , USamplerBuffer
--  , Sampler2DMS
--  , ISampler2DMS
--  , USampler2DMS
--  , Sampler2DMSArray
--  , ISampler2DMSArray
--  , USampler2DMSArray

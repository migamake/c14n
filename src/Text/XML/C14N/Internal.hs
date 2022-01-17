{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.XML.C14N.Internal where

import qualified Data.Map as Map
import           Data.Monoid ((<>), mempty)
import qualified Language.Haskell.TH as TH

import qualified Language.C.Inline as C
import           Language.C.Inline.Context
import qualified Language.C.Types as CT


-- | XML node sets
data LibXMLNodeSet

-- | XML Buffer
data LibXMLBuffer

xmlCtx :: C.Context
xmlCtx = baseCtx <> ctx
  where
    ctx = mempty { ctxTypesTable = xmlTypesTable }

xmlTypesTable :: Map.Map CT.TypeSpecifier TH.TypeQ
xmlTypesTable = Map.fromList
    [ (CT.TypeName "xmlBuffer",  [t| LibXMLBuffer  |])
    , (CT.TypeName "xmlNodeSet", [t| LibXMLNodeSet |])
    ]

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


-- | XML documents
data LibXMLDoc


-- | XML node
data LibXMLNode


-- | XML node sets
data LibXMLNodeSet


-- | XML Buffer
data LibXMLBuffer


xmlCtx :: C.Context
xmlCtx = baseCtx <> bsCtx <> ctx
  where
    ctx = mempty { ctxTypesTable = xmlTypesTable }


xmlTypesTable :: Map.Map CT.TypeSpecifier TH.TypeQ
xmlTypesTable = Map.fromList
    [ (CT.TypeName "xmlDoc",     [t| LibXMLDoc     |])
    , (CT.TypeName "xmlNode",    [t| LibXMLNode    |])
    , (CT.TypeName "xmlNodeSet", [t| LibXMLNodeSet |])
    , (CT.TypeName "xmlBuffer",  [t| LibXMLBuffer  |])
    ]

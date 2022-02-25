--------------------------------------------------------------------------------
-- Haskell bindings for c14n implementation in libxml                         --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Bindings to libxml types and functions required for the c14n 
-- implementation. See http://xmlsoft.org/html/libxml-c14n.html
module Text.XML.C14N.LibXML (
    -- * libxml2 types
    LibXMLDoc,
    LibXMLNode,
    LibXMLNodeSet,
    LibXMLChar,
    LibXMLXPathCtx,
    LibXMLXPathObj,
    LibXMLBuffer,

    -- * Memory-related functions
    freeXml,

    -- * Parsing
    -- See http://xmlsoft.org/html/libxml-parser.html
    xml_opt_recover, 
    xml_opt_noent,
    xml_opt_dtdload,
    xml_opt_dtdattr,
    xml_opt_dtdvalid,
    xml_opt_noerror,
    xml_opt_nowarning,
    xml_opt_pedantic,
    xml_opt_noblanks,
    xml_opt_sax1,
    xml_opt_xinclude,
    xml_opt_nonet,
    xml_opt_nodict,
    xml_opt_nsclean,
    xml_opt_nocdata,
    xml_opt_noxincnode,
    xml_opt_compact,
    xml_opt_old10,
    xml_opt_nobasefix,
    xml_opt_huge,
    xml_opt_oldsax,
    xml_opt_ignore_env,
    xml_opt_big_lines,
    xmlReadMemory,
    htmlReadMemory,
    xmlFreeDoc,

    -- * XML canonicalisation
    -- See http://xmlsoft.org/html/libxml-c14n.html
    c14n_1_0,
    c14n_exclusive_1_0,
    c14n_1_1,
    xmlC14NDocDumpMemory,

    -- * XPath
    -- See http://xmlsoft.org/html/libxml-xpath.html
    xmlXPathNewContext,
    xmlXPathFreeContext,
    xmlXPathEval,
    xmlXPathFreeObject,


    xmlCreateBufferSize,
    xmlBufferContent,
    xmlBufferFree,
    xmlNodeSetDump,
    xmlNodeSetSize,
    xmlNodeSetDumpArr,
    xmlNodeSetMap,

    testLibxml,

    nodePathIdx ,
    nodeByPath ,
    dumpNode ,

    nodeChildren ,
    nodeNext,
    isNullPtr ,
    nodeName
) where 

--------------------------------------------------------------------------------

#include <libxml/parser.h>
#include <libxml/c14n.h>
#include <string.h>

import Data.Word
import Data.Function
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.IO.Unsafe

import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

import qualified Language.C.Inline as C

import Text.XML.C14N.Internal


C.context xmlCtx

C.include "<libxml/tree.h>"
C.include "<libxml/HTMLparser.h>"
C.include "<libxml/xpath.h>"

--------------------------------------------------------------------------------

-- | Original C14N 1.0 specification.
c14n_1_0 :: CInt 
c14n_1_0 = #{const XML_C14N_1_0}

-- | Exclusive C14N 1.0 specification.
c14n_exclusive_1_0 :: CInt 
c14n_exclusive_1_0 = #{const XML_C14N_EXCLUSIVE_1_0}

-- | C14N 1.1 specification.
c14n_1_1 :: CInt 
c14n_1_1 = #{const XML_C14N_1_1}

--------------------------------------------------------------------------------

-- | Recover on errors.
xml_opt_recover :: CInt 
xml_opt_recover = #{const XML_PARSE_RECOVER}

-- | Substitute entities.
xml_opt_noent :: CInt
xml_opt_noent = #{const XML_PARSE_NOENT}

-- | Load the external subset.
xml_opt_dtdload :: CInt 
xml_opt_dtdload = #{const XML_PARSE_DTDLOAD}

-- | Default DTD attributes.
xml_opt_dtdattr :: CInt 
xml_opt_dtdattr = #{const XML_PARSE_DTDATTR}

-- | Validate with the DTD.
xml_opt_dtdvalid :: CInt 
xml_opt_dtdvalid = #{const XML_PARSE_DTDVALID}

-- | Suppress error reports.
xml_opt_noerror :: CInt 
xml_opt_noerror = #{const XML_PARSE_NOERROR}

-- | Suppress warning reports.
xml_opt_nowarning :: CInt 
xml_opt_nowarning = #{const XML_PARSE_NOWARNING}

-- | Pedantic error reporting.
xml_opt_pedantic :: CInt 
xml_opt_pedantic = #{const XML_PARSE_PEDANTIC}

-- | Remove blank nodes.
xml_opt_noblanks :: CInt 
xml_opt_noblanks = #{const XML_PARSE_NOBLANKS}

-- | Use the SAX1 interface internally.
xml_opt_sax1 :: CInt 
xml_opt_sax1 = #{const XML_PARSE_SAX1}

-- | Implement XInclude substitution.
xml_opt_xinclude :: CInt 
xml_opt_xinclude = #{const XML_PARSE_XINCLUDE}

-- | Forbid network access.
xml_opt_nonet :: CInt
xml_opt_nonet = #{const XML_PARSE_NONET}

-- | Do not reuse the context dictionary.
xml_opt_nodict :: CInt
xml_opt_nodict = #{const XML_PARSE_NODICT}

-- | Remove redundant namespaces declarations.
xml_opt_nsclean :: CInt
xml_opt_nsclean = #{const XML_PARSE_NSCLEAN}

-- | Merge CDATA as text nodes.
xml_opt_nocdata :: CInt
xml_opt_nocdata = #{const XML_PARSE_NOCDATA}

-- | Do not generate XINCLUDE START/END nodes.
xml_opt_noxincnode :: CInt
xml_opt_noxincnode = #{const XML_PARSE_NOXINCNODE}

-- | Compact small text nodes; no modification of the tree allowed afterwards
-- (will probably crash if you try to modify the tree)
xml_opt_compact :: CInt
xml_opt_compact = #{const XML_PARSE_COMPACT}

-- | Parse using XML-1.0 before update 5.
xml_opt_old10 :: CInt
xml_opt_old10 = #{const XML_PARSE_OLD10}

-- | Do not fixup XINCLUDE xml:base uris.
xml_opt_nobasefix :: CInt
xml_opt_nobasefix = #{const XML_PARSE_NOBASEFIX}

-- | Relax any hardcoded limit from the parser.
xml_opt_huge :: CInt
xml_opt_huge = #{const XML_PARSE_HUGE}

-- | Parse using SAX2 interface before 2.7.0.
xml_opt_oldsax :: CInt
xml_opt_oldsax = #{const XML_PARSE_OLDSAX}

-- | Ignore internal document encoding hint.
xml_opt_ignore_env :: CInt
xml_opt_ignore_env = #{const XML_PARSE_IGNORE_ENC}

-- | Store big lines numbers in text PSVI field.
xml_opt_big_lines :: CInt
xml_opt_big_lines = #{const XML_PARSE_BIG_LINES}

--------------------------------------------------------------------------------

xml_element_dtd_node :: CInt
xml_element_dtd_node = #{const XML_DTD_NODE}

--------------------------------------------------------------------------------

-- | XML strings
type LibXMLChar = #type xmlChar

-- | XML XPath contexts
data LibXMLXPathCtx

-- | XML XPath objects
data LibXMLXPathObj

-- | Free an XML object.
foreign import ccall unsafe "freeXml"
    freeXml :: Ptr a -> IO ()

-- | Free an XML document.
foreign import ccall unsafe "libxml/tree.h &xmlFreeDoc"
    xmlFreeDoc :: FunPtr ((Ptr LibXMLDoc) -> IO ())

-- | Parses an XML document from a textual representation held in memory.
foreign import ccall unsafe "libxml/parser.h xmlReadMemory"
    xmlReadMemory :: CString 
                  -> CInt 
                  -> CString 
                  -> CString 
                  -> CInt 
                  -> IO (Ptr LibXMLDoc)

-- | Parses an XML document from a textual representation held in memory.
foreign import ccall unsafe "libxml/HTMLparser.h htmlReadMemory"
    htmlReadMemory :: CString   -- ^ buffer
                   -> CInt      -- ^ size of document
                   -> CString   -- ^ URL
                   -> CString   -- ^ encoding
                   -> CInt      -- ^ options
                   -> IO (Ptr LibXMLDoc)

-- | Writes the canonicalised representation of an XML document to memory.
foreign import ccall unsafe "libxml/c14n.h xmlC14NDocDumpMemory"
  xmlC14NDocDumpMemory :: Ptr LibXMLDoc -- ^ The XML document to canonicalise.
                       -> Ptr LibXMLNodeSet -- ^ The nodes set to be included
                                            -- in the canonicalised output.
                       -> CInt -- ^ The canonicalisation mode.
                       -> Ptr (Ptr LibXMLChar) -- ^ A list of inclusive 
                                               -- namespace prefixes
                       -> CInt -- ^ A boolean value indicating whether comments
                               -- should be included in the result or not.
                       -> Ptr (Ptr LibXMLChar) -- ^ A memory address to which
                                               -- the output should be written.
                       -> IO CInt

-- | Creates a new XPath context for the given document.
foreign import ccall unsafe "libxml/xpath.h xmlXPathNewContext"
    xmlXPathNewContext :: Ptr LibXMLDoc -> IO (Ptr LibXMLXPathCtx)

-- | Frees up an 'LibXMLXPathCtx' context.
foreign import ccall unsafe "libxml/xpath.h xmlXPathFreeContext"
    xmlXPathFreeContext :: Ptr LibXMLXPathCtx -> IO ()

-- | 'xmlXPathEval' @pathPtr ctxPtr@ evaluates the XPath location path
-- pointed at by @pathPtr@ in the XPath context pointed at by @ctxPtr@. 
foreign import ccall unsafe "libxml/xpath.h xmlXPathEval"
    xmlXPathEval :: Ptr LibXMLChar 
                 -> Ptr LibXMLXPathCtx 
                 -> IO (Ptr LibXMLXPathObj)

-- | Free up an 'LibXMLXPathObj' object.
foreign import ccall unsafe "libxml/xpath.h xmlXPathFreeObject"
    xmlXPathFreeObject :: Ptr LibXMLXPathObj -> IO ()



-- | Create buffer with given size
foreign import ccall unsafe "libxml/tree.h xmlBufferCreateSize"
    xmlCreateBufferSize :: CInt -> IO (Ptr LibXMLBuffer)


-- | Get buffer content
foreign import ccall unsafe "libxml/tree.h xmlBufferContent"
    xmlBufferContent :: Ptr LibXMLBuffer -> IO CString

-- | Free the buffer
foreign import ccall unsafe "libxml/tree.h xmlBufferFree"
    xmlBufferFree :: Ptr LibXMLBuffer -> IO ()

-- | Get number of items in node set
--
xmlNodeSetSize :: Ptr LibXMLNodeSet -> Int
xmlNodeSetSize nodeSet = fromIntegral $ unsafePerformIO $
    [C.block| int {
        xmlNodeSetPtr nodeSetPtr = $(xmlNodeSet* nodeSet);
        if (NULL == nodeSetPtr)
            return 0;
        else if (nodeSetPtr->nodeNr <= 0)
            return 0;
        else
            return nodeSetPtr->nodeNr;
    }|]


xmlNodeSetDumpArr :: Ptr LibXMLNodeSet
                  -> IO (Vector ByteString)
xmlNodeSetDumpArr nodeSet = do
    V.generateM (xmlNodeSetSize nodeSet) $ \i ->
        let i' = fromIntegral i in
        BS.unsafePackMallocCString =<< [C.block| char* {
            xmlNodeSetPtr nodeSetPtr = $(xmlNodeSet* nodeSet);
            xmlNodePtr node = nodeSetPtr->nodeTab[$(int i')];
            xmlBufferPtr buf = xmlBufferCreateSize(10*1024*1024); // TODO use xmlNodeDumpOutput
            int writtenBytes = xmlNodeDump(buf, NULL, node, 0, 0); // as in `xmllint`
            char* ret;
            if (writtenBytes < 0)
                ret = strdup("?"); // TODO report error
            else
                ret = strdup(xmlBufferContent(buf));
            xmlBufferFree(buf);
            return ret;
        }|]


xmlNodeSetMap :: Ptr LibXMLNodeSet
              -> (Ptr LibXMLNode -> IO a)
              -> IO (Vector a)
xmlNodeSetMap nodeSet mapper =
    V.generateM (xmlNodeSetSize nodeSet) $ \i ->
        let i' = fromIntegral i in
        mapper [C.pure| xmlNode* { $(xmlNodeSet* nodeSet)->nodeTab[$(int i')] } |]


xmlNodeSetDump :: Ptr LibXMLBuffer
               -> Ptr LibXMLNodeSet
               -> IO CInt -- return code, 0 for no errors
                          --              1 for empty node set
                          --              2 for null node set ptr
                          --             <-100 for buffer errors
xmlNodeSetDump buf nodeSet =
    -- TODO `xmlNodeDump` is deprecated, so rewrite with `xmlNodeDumpOutput`
    [C.block| int {
        xmlBufferPtr bufPtr = $(xmlBuffer* buf);
        xmlNodeSetPtr nodeSetPtr = $(xmlNodeSet* nodeSet);
        if (NULL == nodeSetPtr)
            return 1;
        if (nodeSetPtr->nodeNr <= 0)
            return 2;
        for (int i = 0; i < nodeSetPtr->nodeNr; ++i) {
            xmlNodePtr node = nodeSetPtr->nodeTab[i];
            int writtenBytes = xmlNodeDump(bufPtr, NULL, node, 0, 0); // as in `xmllint`
            if (writtenBytes < 0)
                return (writtenBytes - 100);
            xmlBufferWriteChar(bufPtr, "\n");
        }
        xmlBufferWriteChar(bufPtr, "\0");
        return 0;
    }|]


isNullPtr :: Ptr a -> Bool
isNullPtr ptr = ptr == nullPtr -- TODO how to properly check it?


nodeParent :: Ptr LibXMLNode -> Ptr LibXMLNode
nodeParent node = [C.pure| xmlNode* { $(xmlNode* node)->parent } |]


nodeChildren :: Ptr LibXMLNode -> Ptr LibXMLNode
nodeChildren node = [C.pure| xmlNode* { $(xmlNode* node)->children } |]


nodeNext :: Ptr LibXMLNode -> Ptr LibXMLNode
nodeNext node = [C.pure| xmlNode* { $(xmlNode* node)->next } |]

nodeType :: Ptr LibXMLNode -> CInt
nodeType node = [C.pure| int { $(xmlNode* node)->type } |]


nodeName :: Ptr LibXMLNode -> IO ByteString
nodeName node = BS.packCString [C.pure| char const* { $(xmlNode* node)->name } |]

-- | Find node "index path" by node ptr
--
--   NOTE we use `doc` just to ensure that this object still in memory due
--        traversal. For same reason we use unboxed vectors: just to ensure
--        that all calculations have finished before `doc` will be destroyed
--        (because ordinary `Data.Vector` contains lazy references to data)
--
nodePathIdx :: ForeignPtr LibXMLDoc -> Ptr LibXMLNode -> IO (VU.Vector Int) -- TODO remove `IO` here
nodePathIdx doc node = do
    result <- fmap VU.reverse $ VU.unfoldrM go node
    touchForeignPtr doc
    return result
  where
    go :: Ptr LibXMLNode -> IO (Maybe (Int, Ptr LibXMLNode))
    go node
        | isNullPtr node   = return $ Nothing
        | isNullPtr parent = return $ Nothing
        | otherwise        = return $ Just (idx, parent)
      where
        parent = nodeParent node
        firstSibling = nodeChildren $ nodeParent node
        idx = (flip fix) (firstSibling, 0) $ \nxt (n, i) ->
                        if n == node then i
                        else if isNullPtr n then (-1) -- TODO throw error here (or simple `Left something`)
                        else nxt (nodeNext n, if nodeType n == xml_element_dtd_node then i else succ i)

nodeByPath :: Ptr LibXMLDoc -> Vector Int -> Ptr LibXMLNode
nodeByPath doc path
    | V.null path      = nullPtr
    | V.head path /= 0 = nullPtr -- TODO exception
    | otherwise   = V.foldl childByIdx rootNode (V.tail path)
  where
    rootNode = [C.pure| xmlNode* { xmlDocGetRootElement($(xmlDoc* doc)) } |]
    childByIdx :: Ptr LibXMLNode -> Int -> Ptr LibXMLNode
    childByIdx node idx = go idx (nodeChildren node)
      where
        go 0 node = node
        go i node = go (pred i) (nodeNext node)


dumpNode :: Ptr LibXMLDoc -> Ptr LibXMLNode -> IO ()
dumpNode doc node
    | isNullPtr node = return ()
    | otherwise = [C.block| void { xmlElemDump(stdout, $(xmlDoc* doc), $(xmlNode* node)); }|]

-- vim: set ft=haskell :

--------------------------------------------------------------------------------
-- Haskell bindings for c14n implementation in libxml                         --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Bindings to libxml types and functions required for the c14n 
-- implementation. See http://xmlsoft.org/html/libxml-c14n.html
module Text.XML.C14N.LibXML (
    -- * libxml2 types
    LibXMLDoc,
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
    xmlNodeSetDump
) where 

--------------------------------------------------------------------------------

#include <libxml/parser.h>
#include <libxml/c14n.h>

import Data.Word

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types


import qualified Language.C.Inline as C

import Text.XML.C14N.Internal


C.context xmlCtx

C.include "<libxml/tree.h>"
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

-- | XML documents
data LibXMLDoc 


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
foreign import ccall unsafe "libxml/parser.h htmlReadMemory"
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

--------------------------------------------------------------------------------

module Valence.SPARQL.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Text.Parsing.StringParser (Parser)
import Valence.SPARQL.Lexer (anon, blank_node_label, pname_ln, pname_ns)
  
-- | [138]  	BlankNode	  ::=  	BLANK_NODE_LABEL |	ANON
data BlankNode
  = BlankNodeLabel String
  | Anon

instance showBlankNode :: Show BlankNode where
  show Anon = "(Anon)"
  show (BlankNodeLabel label) = "(BlankNodeLabel " <> label <> ")"

instance eqBlankNode :: Eq BlankNode where
  eq Anon Anon = true
  eq (BlankNodeLabel l1) (BlankNodeLabel l2) = l1 == l2
  eq _ _ = false

blankNode :: Parser BlankNode
blankNode = 
   BlankNodeLabel <$> blank_node_label <|>
   Anon <$ anon 


-- | [137]  	PrefixedName	  ::=  	PNAME_LN | PNAME_NS
newtype PrefixedName = PrefixedName {
  namespace :: String
, local     :: Maybe String
}

instance showPrefixedName :: Show PrefixedName where
  show (PrefixedName {namespace : n, local : (Just loc)}) = 
    "(PrefixedName { namespace : " <> n <> ", local : " <> loc <> " })"
  show (PrefixedName {namespace : n, local : Nothing }) = 
    "(PrefixedName { namespace : " <> n <> " })"

instance eqPrefixedName :: Eq PrefixedName where
  eq (PrefixedName {namespace : n1, local : loc1 })  (PrefixedName {namespace : n2, local : loc2 })
    = n1 == n2 && loc1 == loc2
  

prefixedName :: Parser PrefixedName
prefixedName = pnameNs <|> pnameLn 
  where
    pnameLn = do
      ns    <- pname_ns
      local <- pname_ln 
      pure (PrefixedName { namespace : ns, local : (Just local) })
    pnameNs = do
      ns <- pname_ns 
      pure (PrefixedName { namespace : ns, local : Nothing }) 

module Valence.SPARQL.Parser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (fold, many)
import Data.String (singleton, toCharArray)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (option)
import Text.Parsing.StringParser.String (anyDigit, oneOf, satisfy, string)

-- | Lexer 

type LexicalToken = Parser String

-- | [160] ECHAR ::= '\' [tbnrf\"']
echar :: LexicalToken
echar = do
  _ <- string "\\"
  c <- oneOf $ toCharArray "tbdrf\"'" 
  pure ("\\" <> (singleton c))

-- | [161] NIL ::= '(' WS* ')'
nil :: LexicalToken
nil = do
  _ <- string "("
  w <- many ws
  _ <- string ")"
  pure ("(" <> (fold w) <> ")")


-- | [163] ANON ::= '[' WS* ']'
anon :: LexicalToken
anon = do
  _ <- string "["
  w <- many ws
  _ <- string "]"
  pure ("[" <> (fold w) <> "]")


-- | [162] WS ::= #x20|#x9|#xD|#xA
ws :: LexicalToken
ws = singleton <$> satisfy (\c -> 
  c == '\x0020' || 
  c == '\x0009' || 
  c == '\x000D' || 
  c == '\x000A')

-- | [164] PN_CHARS_BASE ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
pn_chars_base :: LexicalToken         
pn_chars_base = singleton <$> (satisfy (\c ->
  (c >= 'A' && c <= 'Z')  ||
  (c >= 'a' && c <= 'z')  ||
  (c >= '\x00C0' && c <= '\x00D6') ||
  (c >= '\x00D8' && c <= '\x00F6') ||
  (c >= '\x00F8' && c <= '\x02FF') ||
  (c >= '\x0370' && c <= '\x037D') ||
  (c >= '\x037F' && c <= '\x1FFF') ||
  (c >= '\x200C' && c <= '\x200D') ||
  (c >= '\x2070' && c <= '\x218F') ||
  (c >= '\x2C00' && c <= '\x2FEF') ||
  (c >= '\x3001' && c <= '\xD7FF') ||
  (c >= '\xF900' && c <= '\xFDCF') ||
  (c >= '\xFDF0' && c <= '\xFFFD') -- ||
  --(c >= '\x10000'&& c <= '\xEFFFF')
))

-- | [165] PN_CHARS_U ::= PN_CHARS_BASE | '_'
pn_chars_u :: LexicalToken
pn_chars_u = pn_chars_base <|> string "_"

-- | [166] VARNAME ::= ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040] )*
varname :: LexicalToken 
varname = (<>) <$> begin <*> (fold <$> (many end))
  where 
    begin = pn_chars_u <|> (singleton <$> anyDigit)
    end = pn_chars_u <|> (singleton <$> satisfy (\c -> 
      (c >= '0' && c <= '9') ||
      c == '\x00B7' ||
      (c >= '\x0300' && c <= '\x036F') || 
      (c >= '\x203F' && c <= '\x2040')))


-- | [167] PN_CHARS ::= PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
pn_chars :: LexicalToken
pn_chars = 
  pn_chars_u <|> 
  string "-" <|> 
  (singleton <$> anyDigit) <|> 
  singleton <$> (satisfy (\c -> c == '\x00B7' || (c >= '\x0300' && c <= '\x036F') || (c >= '\x203F' && c <= '\x2040')))

-- | [168] PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
pn_prefix :: LexicalToken
pn_prefix = (<>) <$> pn_chars_base <*> (option "" moreChars)
  where 
    moreChars = (<>) <$> (fold <$> (many (pn_chars <|> string "."))) <*> pn_chars


-- | [169] PN_LOCAL ::= (PN_CHARS_U | ':' | [0-9] | PLX ) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX) )?
pn_local :: LexicalToken
pn_local = (<>) <$> firstPart <*> (option "" midAndFinal) 
  where
    dotAndMore  = (<>) <$> string "." <*> (try (pn_chars <|> plx <|> string "." <|> string ":"))
    firstPart   = pn_chars_u <|> plx <|> string ":" <|> (singleton <$> anyDigit) 
    middlePart  = many (plx <|> pn_chars <|> dotAndMore <|> string ":")
    finalPart   = option "" (pn_chars <|> plx <|> string ":" )
    midAndFinal = (<>) <$> (fold <$> middlePart) <*> finalPart
  

-- | [170] PLX ::= PERCENT | PN_LOCAL_ESC
plx :: LexicalToken
plx = percent <|> pn_local_esc

-- | [171] PERCENT ::= '%' HEX HEX
percent :: LexicalToken
percent = do
  _   <- string "%"
  h1  <- hex
  h2  <- hex
  pure ("%" <> h1 <> h2)

-- | [172] HEX ::= [0-9] | [A-F] | [a-f]
hex :: LexicalToken
hex = singleton <$> (oneOf $ toCharArray "0123456789abcdefABCDEF")

-- | [173] PN_LOCAL_ESC ::= '\'('_'|'~'|'.'|'-'|'!'|'$'|'&'|"'"|'('|')'|'*'|'+'|' <|>'|';'|'='|'/'|'?'|'#'|'@'| '%' )
pn_local_esc :: LexicalToken
pn_local_esc = 
  string "\\_"  <|> 
  string "\\~"  <|>
  string "\\."  <|>
  string "\\-"  <|>
  string "\\!"  <|>
  string "\\$"  <|>
  string "\\&"  <|>
  string "\\\"" <|>
  string "\\("  <|>
  string "\\)"  <|>
  string "\\*"  <|>
  string "\\+"  <|>
  string "\\,"  <|>
  string "\\;"  <|>
  string "\\="  <|>
  string "\\/"  <|> 
  string "\\?"  <|> 
  string "\\#"  <|> 
  string "\\@"  <|>  
  string "\\%"

module Valence.SPARQL.Parser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (elem, fold, many)
import Data.String (singleton, toCharArray)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (lookAhead, many1, option)
import Text.Parsing.StringParser.String (alphaNum, anyDigit, anyLetter, oneOf, satisfy, string)

-- | Lexer 

type LexicalToken = Parser String

-- | [139]  	IRIREF	  ::=  	'<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
iriref :: LexicalToken
iriref = lAngleBracket <> middlePart <> rAngleBracket
  where
    lAngleBracket = string "<"
    rAngleBracket = string ">"
    middlePart = fold <$> (many $ singleton <$> satisfy (\c -> 
                    (c > '\x0020') &&
                    (not elem c ['<', '>','"', '{', '}', '|', '^', '`', '\\']) 
                  ))

-- | [140]  	PNAME_NS	  ::=  	PN_PREFIX? ':'
pname_ns :: LexicalToken
pname_ns = optional_pn_prefix <> (string ":") 
  where
    optional_pn_prefix = option "" pn_prefix

-- | [141]  	PNAME_LN	  ::=  	PNAME_NS PN_LOCAL
pname_ln :: LexicalToken
pname_ln = pname_ns <> pn_local

-- | [142]  	BLANK_NODE_LABEL	  ::=  	'_:' ( PN_CHARS_U | [0-9] ) ((PN_CHARS|'.')* PN_CHARS)?
blank_node_label :: LexicalToken
blank_node_label = 
  (string "_:" <> (pn_chars_u <|> (singleton <$> anyDigit))) <>
  (option "" ((fold <$> (many (pn_chars <|> ((string ".") <* (lookAhead pn_chars)) )))))


-- | [143]  	VAR1	  ::=  	'?' VARNAME
var1 :: LexicalToken
var1 = string "?" <> varname

-- | [144]  	VAR2	  ::=  	'$' VARNAME
var2 :: LexicalToken
var2 = string "$" <> varname

-- | [145]  	LANGTAG	  ::=  '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*	
langtag :: LexicalToken
langtag = at <> letters <> dashAlphaNums
  where 
    at = string "@"
    letters = fold <$> (many1 $ singleton <$> anyLetter) 
    dashAlphaNums = fold <$> (many ((string "-") <> (fold <$> (many1 (singleton <$> alphaNum)))))

-- | [146] INTEGER ::= [0-9]+
integer :: LexicalToken
integer = fold <$> (many1 $ singleton <$> anyDigit)

-- | [147] DECIMAL ::= [0-9]* '.' [0-9]+
decimal :: LexicalToken
decimal = possiblyDigits <> dot <> integer 

possiblyDigits :: Parser String
possiblyDigits = fold <$> (many $ singleton <$> anyDigit)

dot :: Parser String
dot = string "."

-- | [148] DOUBLE ::= [0-9]+ '.' [0-9]* EXPONENT | '.' ([0-9])+ EXPONENT | ([0-9])+ EXPONENT
double :: LexicalToken
double = try one <|> try two <|> try three 
  where
    one = integer <> dot <> possiblyDigits <> exponent
    two = dot <> integer <> exponent 
    three = integer <> exponent 

-- | [149] INTEGER_POSITIVE ::= '+' INTEGER
integer_positive :: LexicalToken
integer_positive = (string "+") <> integer

-- | [150] DECIMAL_POSITIVE ::= '+' DECIMAL
decimal_positive :: LexicalToken
decimal_positive = (string "+") <> decimal 

-- | [151] DOUBLE_POSITIVE ::= '+' DOUBLE
double_positive :: LexicalToken
double_positive = (string "+") <> double 

-- | [152] INTEGER_NEGATIVE ::= '-' INTEGER
integer_negative :: LexicalToken
integer_negative = (string "-") <> integer

-- | [153] DECIMAL_NEGATIVE ::= '-' DECIMAL
decimal_negative :: LexicalToken
decimal_negative = (string "-") <> decimal

-- | [154] DOUBLE_NEGATIVE ::= '-' DOUBLE
double_negative :: LexicalToken
double_negative = (string "-") <> double 

-- | [155] EXPONENT ::= [eE] [+-]? [0-9]+
exponent :: LexicalToken
exponent = e <> sign <> nums 
  where
    e     = singleton <$> oneOf ['e', 'E']
    sign  = (option "" (singleton <$> oneOf ['-', '+']))
    nums  = fold <$> (many1 $ singleton <$> anyDigit)

-- | [156] STRING_LITERAL1 ::= "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
string_literal1 :: LexicalToken
string_literal1 = string_literal "'" allowedChar
  where 
    allowedChar = anyButThese <|> echar
    anyButThese = singleton <$> (satisfy (\c -> c /= '\x0027' && c /= '\x0005C' && c /= '\x000D'))

-- | [157] STRING_LITERAL2 ::= '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
string_literal2 :: LexicalToken
string_literal2 = string_literal "\"" allowedChar
  where 
    allowedChar = anyButThese <|> echar
    anyButThese = singleton <$> (satisfy (\c -> c /= '\x0022' && c /= '\x0005C' && c /= '\x000D'))

-- | [158] STRING_LITERAL_LONG1 ::= "'''" ( ( "'" | "''" )? ( [^'\] | ECHAR ) )* "'''"
string_literal_long1 :: LexicalToken
string_literal_long1 = string_literal "'''" content
  where
    content = quotes <> (singleton <$> satisfy (\c -> c /= '\'' && c /= '\\') <|> echar) 
    quotes = try (option "" (doubleSingleQuote <|> singleSingleQuote))
    singleSingleQuote = ((string "'") <* (lookAhead (singleton <$> satisfy (\c -> c /= '\''))))
    doubleSingleQuote = (string "''") <* (lookAhead $ satisfy (\c -> c /= '\''))


-- | [159] STRING_LITERAL_LONG2 ::= '"""' ( ( '"' | '""' )? ( [^"\] | ECHAR ) )* '"""'
string_literal_long2 :: LexicalToken
string_literal_long2 = string_literal "\"\"\"" content
  where
    content = quotes <> (singleton <$> satisfy (\c -> c /= '"' && c /= '\\') <|> echar) 
    quotes = try (option "" (doubleSingleQuote <|> singleSingleQuote))
    singleSingleQuote = ((string "\"") <* (lookAhead (singleton <$> satisfy (\c -> c /= '"'))))
    doubleSingleQuote = (string "\"\"") <* (lookAhead $ satisfy (\c -> c /= '"')) 

-- helper for the different string_literals
string_literal :: String -> (Parser String) -> LexicalToken
string_literal quoteType allowedChar = do
  _   <- string quoteType 
  cs  <- fold <$> (many allowedChar)
  _   <- string quoteType 
  pure (quoteType <> cs <> quoteType)

-- | [160] ECHAR ::= '\' [tbnrf\"']
echar :: LexicalToken
echar = (string "\\") <> (singleton <$> (oneOf $ toCharArray "tbdrf\"'"))

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
pn_prefix = pn_chars_base <> (option "" moreChars)
  where 
    moreChars = (fold <$> (many (pn_chars <|> ((string ".") <* (lookAhead pn_chars)))))


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

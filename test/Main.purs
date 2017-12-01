module Test.Main where

import Prelude

import Control.Monad.Aff (liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Gen (chooseInt, suchThat)
import Data.Char (fromCharCode, toCharCode)
import Data.Char.Gen (genAlpha, genDigitChar, genUnicodeChar)
import Data.Either (Either(..), isLeft, isRight)
import Data.List.Lazy (elem, fold, foldMap, replicateM)
import Data.NonEmpty ((:|))
import Data.String (singleton, toCharArray)
import Data.String.Gen (genDigitString)
import Debug.Trace (spy)
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck, (===))
import Test.QuickCheck.Gen (Gen, elements)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Text.Parsing.StringParser (runParser)
import Valence.SPARQL.Parser (SelectClause(..), anon, blank_node_label, decimal, decimal_negative, decimal_positive, double, double_negative, double_positive, echar, exponent, hex, integer, integer_negative, integer_positive, iriref, langtag, nil, percent, plx, pn_chars, pn_chars_base, pn_chars_u, pn_local, pn_local_esc, pn_prefix, pname_ln, pname_ns, selectClause, string_literal1, string_literal2, string_literal_long1, string_literal_long2, var1, var2, varname, ws)


newtype ArbitraryPnLocalEsc = ArbitraryPnLocalEsc String 

instance arbitraryPnLocalEsc :: Arbitrary ArbitraryPnLocalEsc where
  arbitrary = ArbitraryPnLocalEsc <$> (elements ("\\_" :|  ["\\~", "\\.", "\\-", "\\!", "\\$", "\\&", "\\\"", "\\(", "\\)", "\\*", "\\+", "\\,", "\\;", "\\=", "\\/", "\\?", "\\#", "\\@",  "\\%"]))

genDigits :: Gen String
genDigits = elements ("0" :| ["1", "2", "3", "4", "5", "6", "7", "8", "9"])

newtype ArbitraryHex = ArbitraryHex String

instance arbitraryHex :: Arbitrary ArbitraryHex where
  arbitrary = do
    i <- chooseInt 0 2
    ArbitraryHex <$> (case i of 
      0 -> elements ("0" :| ["1", "2", "3", "4", "5", "6", "7", "8", "9"])
      1 -> elements ("a" :| ["b", "c", "d", "e", "f"])
      _ -> elements ("A" :| ["A", "B", "C", "D", "E", "F"]))


newtype ArbitraryPercent = ArbitraryPercent String

instance arbitraryPercent :: Arbitrary ArbitraryPercent where
  arbitrary = do
    (ArbitraryHex h1) <- arbitrary 
    (ArbitraryHex h2) <- arbitrary
    pure (ArbitraryPercent ("%" <> h1 <> h2))


newtype ArbitraryPlx = ArbitraryPlx String

instance arbitraryPlx :: Arbitrary ArbitraryPlx where
  arbitrary = ArbitraryPlx <$> (arbitrary >>= (if _ then (arbitrary <#> (\(ArbitraryPercent p) -> p)) else (arbitrary <#> (\(ArbitraryPnLocalEsc p) -> p)) ))

newtype ArbitraryPnCharBase = ArbitraryPnCharBase String

instance arbitraryPnCharBase :: Arbitrary ArbitraryPnCharBase where
  arbitrary = do
    i <- chooseInt 0 12 
    ArbitraryPnCharBase <$> singleton <$> fromCharCode <$> case i of 
      0   -> intBetweenChars 'a' 'z'
      1   -> intBetweenChars 'A' 'Z'
      2   -> intBetweenChars '\x00C0' '\x00D6'
      3   -> intBetweenChars '\x00D8' '\x00F6'
      4   -> intBetweenChars '\x00F8' '\x02FF'
      5   -> intBetweenChars '\x0370' '\x037D'
      6   -> intBetweenChars '\x037F' '\x1FFF'
      7   -> intBetweenChars '\x200C' '\x200D'
      8   -> intBetweenChars '\x2070' '\x218F'
      9   -> intBetweenChars '\x2C00' '\x2FEF'
      10  -> intBetweenChars '\x3001' '\xD7FF'
      11  -> intBetweenChars '\xF900' '\xFDCF'
      _   -> intBetweenChars '\xFDF0' '\xFFFD'

intBetweenChars :: Char -> Char -> Gen Int
intBetweenChars c1 c2 = chooseInt (toCharCode c1) (toCharCode c2)

newtype ArbitraryPnCharsU = ArbitraryPnCharsU String

instance arbitraryPnCharsU :: Arbitrary ArbitraryPnCharsU where
  arbitrary = ArbitraryPnCharsU <$> (arbitrary >>= (
      if _ 
      then arbitrary <#> (\(ArbitraryPnCharBase c) -> c)
      else pure "_" 
    ))

newtype ArbitraryVarname = ArbitraryVarname String

instance arbitraryVarname :: Arbitrary ArbitraryVarname where
  arbitrary = ArbitraryVarname <$> (
    do 
      begin <-  arbitrary >>= (
                  if _
                  then genDigits 
                  else arbitrary <#> (\(ArbitraryPnCharsU c) -> c)
                )
      n     <- chooseInt 0 10
      end   <- replicateM n (do
                i <- chooseInt 0 4
                case i of 
                  0 -> arbitrary <#> (\(ArbitraryPnCharsU c) -> c)
                  1 -> genDigits 
                  2 -> pure "\x00B7"
                  3 -> singleton <$> fromCharCode <$> intBetweenChars '\x0300' '\x036F'
                  _ -> singleton <$> fromCharCode <$> intBetweenChars '\x203F' '\x2040')
      pure (begin <> (fold end)))

newtype ArbitraryPnChars = ArbitraryPnChars String

instance arbitraryPnChars :: Arbitrary ArbitraryPnChars where
  arbitrary = ArbitraryPnChars <$> do
    i <- chooseInt 0 4
    case i of 
       0 -> arbitrary <#> (\(ArbitraryPnCharsU c) -> c)
       1 -> genDigits 
       2 -> pure "\x00B7"
       2 -> pure "-" 
       3 -> singleton <$> fromCharCode <$> intBetweenChars '\x0300' '\x036F'
       _ -> singleton <$> fromCharCode <$> intBetweenChars '\x203F' '\x2040'

newtype ArbitraryPnPrefix = ArbitraryPnPrefix String

instance arbitraryPnPrefix :: Arbitrary ArbitraryPnPrefix where
  arbitrary = ArbitraryPnPrefix <$> do
    b <- arbitrary <#> (\(ArbitraryPnCharBase b) -> b)
    i <- chooseInt 0 10
    cs <- replicateM i (arbitrary >>= 
            if _ 
            then arbitrary <#> (\(ArbitraryPnChars c) -> "." <> c) 
            else arbitrary <#> (\(ArbitraryPnChars c) -> c)) 
    case i of 
      0 -> pure b
      _ -> arbitrary <#> (\(ArbitraryPnChars c) -> b <> c <> (fold cs))
      

newtype ArbitraryPnLocal = ArbitraryPnLocal String

instance arbitraryPnLocal :: Arbitrary ArbitraryPnLocal where
  arbitrary = do
    i         <- chooseInt 0 3
    firstPart <- case i of 
                  0 -> arbitrary <#> (\(ArbitraryPnCharsU c) -> c)
                  1 -> arbitrary <#> (\(ArbitraryPlx c) -> c) 
                  2 -> genDigitString 
                  _ -> pure ":" 
    n <- chooseInt 0 10
    midPart   <-  fold <$> replicateM n (do
                      n1 <- chooseInt 0 3 
                      case n1 of 
                        0 -> arbitrary <#> (\(ArbitraryPnChars c) -> c)
                        1 -> arbitrary <#> (\(ArbitraryPlx c) -> c) 
                        2 -> pure "."
                        _ -> pure ":")
    endPart   <- case n of 
                  0 -> pure ""
                  _ -> do 
                        n2 <- chooseInt 0 2
                        case n2 of 
                          0 -> arbitrary <#> (\(ArbitraryPnChars c) -> c)
                          1 -> arbitrary <#> (\(ArbitraryPlx p) -> p)
                          _ -> pure ":"
    pure $ ArbitraryPnLocal (firstPart <> midPart <> endPart)
    

newtype ArbitraryWS = ArbitraryWS String

instance arbitraryWS :: Arbitrary ArbitraryWS where
  arbitrary = ArbitraryWS <$> singleton <$> elements ('\x0020' :| ['\x0009','\x000D', '\x000A'])

newtype ArbitraryAnon = ArbitraryAnon String

instance arbitraryAnon :: Arbitrary ArbitraryAnon where
  arbitrary = do
    i <- chooseInt 0 20
    w <- replicateM i (arbitrary <#> (\(ArbitraryWS w) -> w) )
    pure (ArbitraryAnon ("[" <> (fold w) <> "]"))

newtype ArbitraryNil = ArbitraryNil String

instance arbitraryNil :: Arbitrary ArbitraryNil where
  arbitrary = do
    i <- chooseInt 0 20
    w <- replicateM i (arbitrary <#> (\(ArbitraryWS w) -> w) )
    pure (ArbitraryNil ("(" <> (fold w) <> ")"))

newtype AEChar = AEChar String 

instance aEChar :: Arbitrary AEChar where
  arbitrary = do
    c <- elements ("t" :| ["b", "r", "f", "\"", "'"])
    pure $ AEChar ("\\" <> c)

newtype AStringLiteral1 = AStringLiteral1 String
instance aStringLiteral :: Arbitrary AStringLiteral1 where
  arbitrary = do
    i <- chooseInt 0 50
    c <- replicateM i (genUnicodeChar `suchThat` (\c -> not (elem c ['\x0027', '\x0005C', '\x000D'] )))
    pure (AStringLiteral1 ("'" <> (foldMap singleton c) <> "'"))

newtype AStringLiteral2 = AStringLiteral2 String
instance aStringLiteral2 :: Arbitrary AStringLiteral2 where
  arbitrary = do
    i <- chooseInt 0 50
    c <- replicateM i (genUnicodeChar `suchThat` (\c -> not (elem c ['\x0022', '\x0005C', '\x000D'])))
    pure (AStringLiteral2 ("\"" <> (foldMap singleton c) <> "\""))

newtype AStringLiteralLong1 = AStringLiteralLong1 String
instance aStringLiteralLong1 :: Arbitrary AStringLiteralLong1 where
  arbitrary = do
    i <- chooseInt 0 50
    c <- replicateM i (genUnicodeChar `suchThat` (\c -> not (elem c ['\'', '\\'] )))
    pure (AStringLiteralLong1 ("'''" <> (foldMap singleton c) <> "'''"))

newtype AStringLiteralLong2 = AStringLiteralLong2 String
instance aStringLiteralLong2 :: Arbitrary AStringLiteralLong2 where
  arbitrary = do
    i <- chooseInt 0 50
    c <- replicateM i (genUnicodeChar `suchThat` (\c -> not (elem c ['"', '\\'] )))
    pure (AStringLiteralLong2 ("\"\"\"" <> (foldMap singleton c) <> "\"\"\""))

newtype AExponent = AExponent String

instance aExponent :: Arbitrary AExponent where
  arbitrary = do
    e <- elements ("e" :| ["E"])
    s <- elements ("-" :| ["+", ""])
    n <- genDigitString
    pure $ AExponent (e <> s <> n)

newtype AInteger = AInteger String

instance aInteger :: Arbitrary AInteger where
  arbitrary = AInteger <$> genDigitString


newtype ADecimal = ADecimal String

instance aDecimal :: Arbitrary ADecimal where
  arbitrary = do
    i           <- chooseInt 0 5
    firstPart   <- replicateM i genDigitChar
    n           <- chooseInt 1 5
    secondPart  <- replicateM n genDigitChar
    pure (ADecimal ((foldMap singleton firstPart) <> "." <> (foldMap singleton secondPart)))

newtype ADouble = ADouble String

instance aDouble :: Arbitrary ADouble where
  arbitrary = do
    n     <- chooseInt 0 2
    case n of 
      0 -> do 
        i <- arbitrary <#> (\(AInteger int) -> int)
        p <- arbitrary >>= (\b -> if b then (pure "") else (arbitrary <#> (\(AInteger int) -> int)))
        e <- arbitrary <#> (\(AExponent e) -> e)
        pure (ADouble (i <> p <> e))
      1 -> do 
        i <- arbitrary <#> (\(AInteger int) -> int)
        e <- arbitrary <#> (\(AExponent e) -> e)
        pure (ADouble ("." <> i <> e))
      _ -> do 
        i <- arbitrary <#> (\(AInteger int) -> int)
        e <- arbitrary <#> (\(AExponent e) -> e)
        pure (ADouble (i <> e))

newtype AIntegerPositive = AIntegerPositive String

instance aAIntegerPositive :: Arbitrary AIntegerPositive where
  arbitrary = arbitrary <#> (\(AInteger n) -> AIntegerPositive ("+" <> n))

newtype ADecimalPositive = ADecimalPositive String

instance aADecimalPositive :: Arbitrary ADecimalPositive where
  arbitrary = arbitrary <#> (\(ADecimal n) -> ADecimalPositive ("+" <> n))

newtype ADoublePositive = ADoublePositive String

instance aADoublePositive :: Arbitrary ADoublePositive where
  arbitrary = arbitrary <#> (\(ADouble n) -> ADoublePositive ("+" <> n))

newtype AIntegerNegative = AIntegerNegative String

instance aAIntegerNegative :: Arbitrary AIntegerNegative where
  arbitrary = arbitrary <#> (\(AInteger n) -> AIntegerNegative ("-" <> n))

newtype ADecimalNegative = ADecimalNegative String

instance aADecimalNegative :: Arbitrary ADecimalNegative where
  arbitrary = arbitrary <#> (\(ADecimal n) -> ADecimalNegative ("-" <> n))

newtype ADoubleNegative = ADoubleNegative String

instance aADoubleNegative :: Arbitrary ADoubleNegative where
  arbitrary = arbitrary <#> (\(ADouble n) -> ADoubleNegative ("-" <> n))

newtype ALangtag = ALangtag String

instance aLangtag :: Arbitrary ALangtag where
  arbitrary = do
    lCount        <- chooseInt 1 5
    letters       <- replicateM lCount genAlpha
    ldCount       <- chooseInt 0 5
    letterDashes  <- replicateM ldCount (elements ('a' :| (toCharArray "bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")))
    case ldCount of 
      0 -> pure (ALangtag ("@" <> (foldMap singleton letters) <> (foldMap singleton letterDashes)))
      _ -> pure (ALangtag ("@" <> (foldMap singleton letters) <> "-" <> (foldMap singleton letterDashes)))


newtype AVar2 = AVar2 String

instance aVar2 :: Arbitrary AVar2 where
  arbitrary = do
    v <- arbitrary <#> (\(ArbitraryVarname v) -> v)
    pure (AVar2 ("$" <> v))

newtype AVar1 = AVar1 String

instance aVar1 :: Arbitrary AVar1 where
  arbitrary = do
    v <- arbitrary <#> (\(ArbitraryVarname v) -> v)
    pure (AVar1 ("?" <> v))

newtype ABlankNodeLabel = ABlankNodeLabel String

instance aBlankNodeLabel :: Arbitrary ABlankNodeLabel where
  arbitrary = do
    firstPart   <-  arbitrary >>= (\b ->
                      if b 
                      then (arbitrary <#> (\(ArbitraryPnCharsU c) -> c))
                      else singleton <$> genDigitChar)
    secondPart  <-  do
                      i   <- chooseInt 0 5
                      cs  <- replicateM i (arbitrary >>= (\b ->
                              if b 
                              then (arbitrary <#> (\(ArbitraryPnChars c) -> c))
                              else (arbitrary <#> (\(ArbitraryPnChars c) -> "." <> c))))
                      pure (fold cs)
    pure (ABlankNodeLabel ("_:" <> firstPart <> secondPart))


newtype APNameNS = APNameNS String

instance aPNameNS :: Arbitrary APNameNS where
  arbitrary = arbitrary >>= (\b -> 
                if b 
                then  (arbitrary <#> (\(ArbitraryPnPrefix p) -> APNameNS (p <> ":")))
                else pure $ APNameNS ":"
               ) 

newtype APNameLN = APNameLN String   

instance aPNameLN :: Arbitrary APNameLN where
  arbitrary = do
    ns  <- arbitrary <#> (\(APNameNS n) -> n) 
    loc <- arbitrary <#> (\(ArbitraryPnLocal l) -> l)
    pure (APNameLN (ns <> loc))

newtype AIRIRef = AIRIRef String

instance aIRIRef :: Arbitrary AIRIRef where
    arbitrary = do
      i <- chooseInt 0 200
      c <- replicateM i (genUnicodeChar `suchThat` (\c -> 
                          (c > '\x0020') && 
                          (not (elem c ['<', '>','"', '{', '}', '|', '^', '`', '\\', '\t', '\n', '\r']))  
                         ))
      pure (AIRIRef ("<" <> (foldMap singleton c) <> ">"))


main :: Eff (QCRunnerEffects () ) Unit  
main = run [consoleReporter] do 
  describe "Valence.SPARQL.Parser" do

    describe "parses [173] PN_LOCAL_ESC" do 
      it "should prase the common case" do
        (runParser pn_local_esc "\\_" ) `shouldEqual` (Right "\\_")

      it "should fail on some invalid input" do 
        (isLeft $ (runParser pn_local_esc "_" )) `shouldEqual` true
        (isLeft $ (runParser pn_local_esc "$" )) `shouldEqual` true
        (isLeft $ (runParser pn_local_esc "\"")) `shouldEqual` true

      it "should pass quickChec" do
        liftEff' ( quickCheck (\(ArbitraryPnLocalEsc pn) -> (runParser pn_local_esc pn ) === (Right pn)))

    describe "parses [172] HEX" do
      it "should parse common case" do
        (runParser hex "0") `shouldEqual` (Right "0")
        (runParser hex "a") `shouldEqual` (Right "a")
        (runParser hex "f") `shouldEqual` (Right "f")
        (runParser hex "A") `shouldEqual` (Right "A")
        (runParser hex "F") `shouldEqual` (Right "F")

      it "should fail on invalid cases" do
        (isLeft $ (runParser hex "G")) `shouldEqual` true
        (isLeft $ (runParser hex "g")) `shouldEqual` true
        (isLeft $ (runParser hex "!")) `shouldEqual` true

      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryHex h) -> (runParser hex h) === (Right h))) 

    describe "parse [171] PERCENT" do
      it "should parse the common case" do
        (runParser percent "%00") `shouldEqual` (Right "%00")
        (runParser percent "%a0") `shouldEqual` (Right "%a0")
        (runParser percent "%A0") `shouldEqual` (Right "%A0")
        (runParser percent "%af") `shouldEqual` (Right "%af")
        (runParser percent "%AF") `shouldEqual` (Right "%AF")

      it "should fail on some invalid input" do
        (isLeft $ (runParser percent "%a")) `shouldEqual` true
        (isLeft $ (runParser percent "%aG")) `shouldEqual` true
        (isLeft $ (runParser percent "af")) `shouldEqual` true

      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryPercent p) -> (runParser percent p) === (Right p)))

    describe "parse [170] plx" do
      it "should parse the common case" do 
        (runParser plx "\\_")  `shouldEqual` (Right "\\_")
        (runParser plx "%00")  `shouldEqual` (Right "%00")

      it "should fail on invalid input" do
        (isLeft $ (runParser plx "_")) `shouldEqual` true
        (isLeft $ (runParser plx "00")) `shouldEqual` true

      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryPlx p) -> (runParser plx p)  === (Right p))) 

    describe "parse [164] pn_char_base" do
      it "should parse some base character" do
        (runParser pn_chars_base "a") `shouldEqual` (Right "a")

      it "should fail on invalid input" do 
        (isLeft $ (runParser pn_chars_base "\x0008")) `shouldEqual` true

      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryPnCharBase c) -> (runParser pn_chars_base c) === (Right c)))

    describe "parse [165] pn_chars_u" do 
      it "should parse some valid characters" do
        (runParser pn_chars_u "a") `shouldEqual` (Right "a")
        (runParser pn_chars_u "_") `shouldEqual` (Right "_")

      it "should fail on invalid input" do 
        (isLeft $ (runParser pn_chars_u "\x0008")) `shouldEqual` true

      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryPnCharsU c) -> (runParser pn_chars_u c) === (Right c)))

    describe "parse [166] varname" do
      it "should parse some valid input" do 
        (runParser varname "asdf") `shouldEqual` (Right "asdf")
        (runParser varname "9asdf") `shouldEqual` (Right "9asdf")
        (runParser varname "9\x00B7\x0300\x203F") `shouldEqual` (Right "9\x00B7\x0300\x203F")

      it "should fail on invalid input" do
        (isLeft (runParser varname "\t")) `shouldEqual` true 

      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryVarname v) -> (runParser varname v) === (Right v)))

    describe "parse [167] pn_chars" do
      it "should parse some valid input" do
        (runParser pn_chars "a") `shouldEqual` (Right "a")
        (runParser pn_chars "0") `shouldEqual` (Right "0")
        (runParser pn_chars "-") `shouldEqual` (Right "-")
        (runParser pn_chars "\x0300") `shouldEqual` (Right "\x0300")

      it "shoudl fail on invalid input" do
        (isLeft (runParser pn_chars "\t")) `shouldEqual` true 
      
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryPnChars c) -> (runParser pn_chars c) === (Right c)))

    describe "parse [169] pn_local" do
      it "should parse some valid input" do
        (runParser pn_local "a") `shouldEqual` (Right "a")
        (runParser pn_local "_") `shouldEqual` (Right "_")
        (runParser pn_local "asdf") `shouldEqual` (Right "asdf")
        (runParser pn_local "a:sdf") `shouldEqual` (Right "a:sdf")
        (runParser pn_local "asdf\\%") `shouldEqual` (Right "asdf\\%")
        (runParser pn_local "a.sdf") `shouldEqual` (Right "a.sdf")
        (runParser pn_local "a..sdf") `shouldEqual` (Right "a..sdf")

      it "should fail on invalid input" do
        (isLeft (runParser pn_local "\t")) `shouldEqual` true 

      it "should pass quickCheck" do 
        liftEff' (quickCheck (\(ArbitraryPnLocal c) -> (runParser pn_local c) === (Right c)))

    describe "parse [162] ws" do
      it "should parse valid content" do
        (runParser ws "\x0020") `shouldEqual` (Right "\x0020")
        (runParser ws "\x0009") `shouldEqual` (Right "\x0009")
        (runParser ws "\x000D") `shouldEqual` (Right "\x000D")
        (runParser ws "\x000A") `shouldEqual` (Right "\x000A")

      it "should fail on invalid input" do
        (isLeft (runParser ws "a") `shouldEqual` true)

    describe "parse [163] anon" do
      it "should parse valid content" do
        (runParser anon "[ ]") `shouldEqual` (Right "[ ]")
        (runParser anon "[]") `shouldEqual` (Right "[]")
      
      it "should fail on invalid input" do
        (isLeft (runParser anon "[ ") `shouldEqual` true) 

    describe "parse [161] nil" do
      it "should parse valid content" do
        (runParser nil "( )") `shouldEqual` (Right "( )")
        (runParser nil "()") `shouldEqual` (Right "()")
      
      it "should fail on invalid input" do
        (isLeft (runParser nil "( ") `shouldEqual` true) 

    describe "parse [160] echar" do
      it "should parse valid content" do
        (runParser echar "\\t") `shouldEqual` (Right "\\t")
        (runParser echar "\\b") `shouldEqual` (Right "\\b")
        (runParser echar "\\d") `shouldEqual` (Right "\\d")
        (runParser echar "\\r") `shouldEqual` (Right "\\r")
        (runParser echar "\\f") `shouldEqual` (Right "\\f")
        (runParser echar "\\\"") `shouldEqual` (Right "\\\"")
        (runParser echar "\\'") `shouldEqual` (Right "\\'")
      
      it "should fail on invalid input" do
        (isLeft (runParser echar "t") `shouldEqual` true) 

    describe "parse [156] string_literal1" do
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(AStringLiteral1 s) -> (runParser string_literal1 s) === (Right s)))

    describe "parse [157] string_literal2" do
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(AStringLiteral2 s) -> (runParser string_literal2 s) === (Right s)))

    describe "parse [158] string_literal_long1" do
      it "should parse valid input" do 
        (runParser string_literal_long1 "'''a'''") `shouldEqual` (Right "'''a'''")
      it "should handle an empty string" do
        (runParser string_literal_long1 "''''''") `shouldEqual` (Right "''''''")
      it "should handle a single quotes followed by another character" do
        (runParser string_literal_long1 "''''a'''") `shouldEqual` (Right "''''a'''")
      it "should handle a two single quotes followed by another character" do
        (runParser string_literal_long1 "'''''a'''") `shouldEqual` (Right "'''''a'''")
      it "should handle a single quotes followed by an echar" do
        (runParser string_literal_long1 "''''\\t'''") `shouldEqual` (Right "''''\\t'''")
      it "should handle a two single quotes followed by an echar" do
        (runParser string_literal_long1 "'''''\\t'''") `shouldEqual` (Right "'''''\\t'''")
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(AStringLiteralLong1 s) -> (runParser string_literal_long1 s) === (Right s)))

    describe "parse [159] string_literal_long2" do
      it "should parse valid input" do 
        (runParser string_literal_long2 "\"\"\"a\"\"\"") `shouldEqual` (Right "\"\"\"a\"\"\"")
      it "should handle an empty string" do
        (runParser string_literal_long2 "\"\"\"\"\"\"") `shouldEqual` (Right "\"\"\"\"\"\"")
      it "should handle a single quotes followed by another character" do
        (runParser string_literal_long2 "\"\"\"\"a\"\"\"") `shouldEqual` (Right "\"\"\"\"a\"\"\"")
      it "should handle a two single quotes followed by another character" do
        (runParser string_literal_long2 "\"\"\"\"\"a\"\"\"") `shouldEqual` (Right "\"\"\"\"\"a\"\"\"")
      it "should handle a single quotes followed by an echar" do
        (runParser string_literal_long2 "\"\"\"\"\\t\"\"\"") `shouldEqual` (Right "\"\"\"\"\\t\"\"\"")
      it "should handle a two single quotes followed by an echar" do
        (runParser string_literal_long2 "\"\"\"\"\"\\t\"\"\"") `shouldEqual` (Right "\"\"\"\"\"\\t\"\"\"")
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(AStringLiteralLong2 s) -> (runParser string_literal_long2 s) === (Right s)))

    describe "parse [155] exponent" do
      it "should parse basic valid input" do
        (runParser exponent "e+100") `shouldEqual` (Right "e+100")
        (runParser exponent "E-100") `shouldEqual` (Right "E-100")
        (runParser exponent "e100") `shouldEqual` (Right "e100")
      it "should fail on some invalid input" do
        (isLeft (runParser exponent "-100")) `shouldEqual` true
        (isLeft (runParser exponent "e-")) `shouldEqual` true
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(AExponent e) -> (runParser exponent e) === (Right e)))

    describe "parse [146] integer" do
      it "should parse some basic input" do
        (runParser integer "123456") `shouldEqual` (Right "123456")
      it "should fail on invalid input" do
        (isLeft (runParser integer "asdf")) `shouldEqual` true
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(AInteger i) -> (runParser integer i)  === (Right i)))

    describe "parse [147] decimal" do
      it "should parse some basic input" do
        (runParser decimal "123.456") `shouldEqual` (Right "123.456")
        (runParser decimal ".456") `shouldEqual` (Right ".456")
      it "should fail on invalid input" do
        (isLeft (runParser decimal "asdf")) `shouldEqual` true
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ADecimal d) -> (runParser decimal d)  === (Right d)))

    describe "parse [148] double" do
      it "should parse some basic input" do
        (runParser double "123.456e1") `shouldEqual` (Right "123.456e1")
        (runParser double ".456e10") `shouldEqual` (Right ".456e10")
        (runParser double "456E100") `shouldEqual` (Right "456E100")
      it "should fail on invalid input" do
        (isLeft (runParser double "asdf")) `shouldEqual` true
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ADouble d) -> (runParser double d)  === (Right d)))

    describe "parse [146] integer_positive" do
      it "should parse some basic input" do
        (runParser integer_positive "+123456") `shouldEqual` (Right "+123456")
      it "should fail on invalid input" do
        (isLeft (runParser integer_positive "asdf")) `shouldEqual` true
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(AIntegerPositive i) -> (runParser integer_positive i)  === (Right i)))

    describe "parse [150] decimal_positive" do
      it "should parse some basic input" do
        (runParser decimal_positive "+123.456") `shouldEqual` (Right "+123.456")
        (runParser decimal_positive "+.456") `shouldEqual` (Right "+.456")
      it "should fail on invalid input" do
        (isLeft (runParser decimal_positive "asdf")) `shouldEqual` true
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ADecimalPositive d) -> (runParser decimal_positive d)  === (Right d)))

    describe "parse [151] double_positive" do
      it "should parse some basic input" do
        (runParser double_positive "+123.456e1") `shouldEqual` (Right "+123.456e1")
        (runParser double_positive "+.456e10") `shouldEqual` (Right "+.456e10")
        (runParser double_positive "+456E100") `shouldEqual` (Right "+456E100")
      it "should fail on invalid input" do
        (isLeft (runParser double_positive "asdf")) `shouldEqual` true
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ADoublePositive d) -> (runParser double_positive d)  === (Right d)))

    describe "parse [152] integer_negative" do
      it "should parse some basic input" do
        (runParser integer_negative "-123456") `shouldEqual` (Right "-123456")
      it "should fail on invalid input" do
        (isLeft (runParser integer_negative "asdf")) `shouldEqual` true
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(AIntegerNegative i) -> (runParser integer_negative i)  === (Right i)))

    describe "parse [153] decimal_negative" do
      it "should parse some basic input" do
        (runParser decimal_negative "-123.456") `shouldEqual` (Right "-123.456")
        (runParser decimal_negative "-.456") `shouldEqual` (Right "-.456")
      it "should fail on invalid input" do
        (isLeft (runParser decimal_negative "asdf")) `shouldEqual` true
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ADecimalNegative d) -> (runParser decimal_negative d)  === (Right d)))

    describe "parse [154] double_negative" do
      it "should parse some basic input" do
        (runParser double_negative "-123.456e1") `shouldEqual` (Right "-123.456e1")
        (runParser double_negative "-.456e10") `shouldEqual` (Right "-.456e10")
        (runParser double_negative "-456E100") `shouldEqual` (Right "-456E100")
      it "should fail on invalid input" do
        (isLeft (runParser double_negative "asdf")) `shouldEqual` true
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ADoubleNegative d) -> (runParser double_negative d)  === (Right d)))

    describe "parse [145] langtag" do
      it "should parse some basic input" do
        (runParser langtag "@eng") `shouldEqual` (Right "@eng")
        (runParser langtag "@eng-123asdf") `shouldEqual` (Right "@eng-123asdf")
      it "should fail on bad data" do 
        (isLeft (runParser langtag "eng")) `shouldEqual` true
      it "should pass quickCheck" do 
        liftEff' (quickCheck (\(ALangtag t) -> (runParser langtag t)  === (Right t)))

    describe "parse [144] var2" do
      it "parse basic input" do
        liftEff' (quickCheck (\(AVar2 v) -> (runParser var2 v) === (Right v)))

    describe "parse [143] var1" do
      it "parse basic input" do
        liftEff' (quickCheck (\(AVar1 v) -> (runParser var1 v) === (Right v)))

    describe "parse [142] blank_node_label" do
      it "should parse some basic input" do
        (runParser blank_node_label "_:a") `shouldEqual` (Right "_:a")
        (runParser blank_node_label "_:asdf") `shouldEqual` (Right "_:asdf")  
      it "should fail on invalid input" do 
        (isLeft (runParser blank_node_label "a")) `shouldEqual` true 
        (isLeft (runParser blank_node_label ":_a.")) `shouldEqual` true 
      it "should pass quickCheck" do 
        liftEff' (quickCheck (\(ABlankNodeLabel b) -> (runParser blank_node_label b) === Right(b))) 

    describe "parse [168] pn_prefix" do 
      it "should parse some valid input" do
        (runParser pn_prefix "asdf")  `shouldEqual` (Right "asdf")
      it "should fail on some invalid input" do 
        (isLeft (runParser pn_prefix "0"))  `shouldEqual` true 
      it "should pass quickCheck" do 
        liftEff' (quickCheck (\(ArbitraryPnPrefix p) -> (runParser pn_prefix p) === (Right p)))

    describe "parse [141] pname_ln" do
      it "should parse valid input" do 
        (runParser pname_ln "a:a")  `shouldEqual` (Right "a:a")
        (runParser pname_ln "asdf:asdf")  `shouldEqual` (Right "asdf:asdf")
      it "should fail on invalid input" do 
        (isLeft (runParser pname_ln "a:"))  `shouldEqual` true
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(APNameLN n) -> (runParser pname_ln n) === (Right n)))

    describe "parse [140] pname_ns" do
      it "should parse some basic input" do
        (runParser pname_ns ":") `shouldEqual` (Right ":")
        (runParser pname_ns "a:") `shouldEqual` (Right "a:")
        (runParser pname_ns "asdf:") `shouldEqual` (Right "asdf:")
      it "should fail on invalid input" do 
        (isLeft (runParser pname_ns "a")) `shouldEqual` true
        (isLeft (runParser pname_ns "0:")) `shouldEqual` true
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(APNameNS n) -> (runParser pname_ns n) === Right(n))) 

    describe "parse [139] iriref" do
      it "should parse some basic input" do
        (runParser iriref "<a>") `shouldEqual` (Right "<a>")
        (runParser iriref "<http://slashdot.org>") `shouldEqual` (Right "<http://slashdot.org>")
      it "should fail on some invalid input" do
        (isLeft (runParser iriref "<a")) `shouldEqual` true 
        (isLeft (runParser iriref "<<>")) `shouldEqual` true 
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(AIRIRef i) -> (runParser iriref i) === Right(i))) 


    -- | Parsing

    describe "parse some SPARQL query" do 
      it "parse a simple query" do 
        let query = """SELECT ?title ?author
         WHERE
          {
            <http://example.org/book/book1> <http://purl.org/dc/elements/1.1/title> ?title .
          }
        """ 
        (runParser selectClause query) `shouldEqual` (Right (SelectClause ["?title", "?author"]))
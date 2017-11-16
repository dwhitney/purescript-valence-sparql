module Test.Main where

import Prelude

import Control.Monad.Aff (liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Gen (chooseInt)
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..), isLeft)
import Data.List.Lazy (fold, replicateM)
import Data.NonEmpty ((:|))
import Data.String (drop, singleton)
import Test.QuickCheck (class Arbitrary, Result(..), arbitrary, quickCheck, (===))
import Test.QuickCheck.Gen (Gen, elements)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Text.Parsing.StringParser (runParser)
import Valence.SPARQL.Parser (hex, percent, plx, pn_chars, pn_chars_base, pn_chars_u, pn_local_esc, varname)


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
      it "should runParser common case" do
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

    describe "runParser [171] PERCENT" do
      it "should runParser the common case" do
        (runParser percent "%00") `shouldEqual` (Right "00")
        (runParser percent "%a0") `shouldEqual` (Right "a0")
        (runParser percent "%A0") `shouldEqual` (Right "A0")
        (runParser percent "%af") `shouldEqual` (Right "af")
        (runParser percent "%AF") `shouldEqual` (Right "AF")

      it "should fail on some invalid input" do
        (isLeft $ (runParser percent "%a")) `shouldEqual` true
        (isLeft $ (runParser percent "%aG")) `shouldEqual` true
        (isLeft $ (runParser percent "af")) `shouldEqual` true

      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryPercent p) -> (runParser percent p) === (Right (drop 1 p))))

    describe "runParser [170] plx" do
      it "should runParser the common case" do 
        (runParser plx "\\_")  `shouldEqual` (Right "\\_")
        (runParser plx "%00")  `shouldEqual` (Right "00")

      it "should fail on invalid input" do
        (isLeft $ (runParser plx "_")) `shouldEqual` true
        (isLeft $ (runParser plx "00")) `shouldEqual` true

      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryPlx p) -> (case ((runParser plx p)  === (Right p)) of
          Failed _ -> (runParser plx p) === (Right (drop 1 p))
          Success  -> Success)))

    describe "runParser [164] pn_char_base" do
      it "should runParser some base character" do
        (runParser pn_chars_base "a") `shouldEqual` (Right "a")

      it "should fail on invalid input" do 
        (isLeft $ (runParser pn_chars_base "\x0008")) `shouldEqual` true

      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryPnCharBase c) -> (runParser pn_chars_base c) === (Right c)))

    describe "runParser [165] pn_chars_u" do 
      it "should runParser some valid characters" do
        (runParser pn_chars_u "a") `shouldEqual` (Right "a")
        (runParser pn_chars_u "_") `shouldEqual` (Right "_")

      it "should fail on invalid input" do 
        (isLeft $ (runParser pn_chars_u "\x0008")) `shouldEqual` true

      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryPnCharsU c) -> (runParser pn_chars_u c) === (Right c)))

    describe "runParser [166] varname" do
      it "should runParser some valid input" do 
        (runParser varname "asdf") `shouldEqual` (Right "asdf")
        (runParser varname "9asdf") `shouldEqual` (Right "9asdf")
        (runParser varname "9\x00B7\x0300\x203F") `shouldEqual` (Right "9\x00B7\x0300\x203F")

      it "should fail on invalid input" do
        (isLeft (runParser varname "\t")) `shouldEqual` true 

      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryVarname v) -> (runParser varname v) === (Right v)))

    describe "runParser [167] pn_chars" do
      it "should runParser some valid input" do
        (runParser pn_chars "a") `shouldEqual` (Right "a")
        (runParser pn_chars "0") `shouldEqual` (Right "0")
        (runParser pn_chars "-") `shouldEqual` (Right "-")
        (runParser pn_chars "\x0300") `shouldEqual` (Right "\x0300")

      it "shoudl fail on invalid input" do
        (isLeft (runParser pn_chars "\t")) `shouldEqual` true 
      
      it "should pass quickCheck" do
        liftEff' (quickCheck (\(ArbitraryPnChars c) -> (runParser pn_chars c) === (Right c)))
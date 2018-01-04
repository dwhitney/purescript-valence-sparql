module Test.Parser where
  
import Prelude

import Control.Monad.Aff (liftEff')
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Gen (chooseInt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Test.Lexer (ABlankNodeLabel(ABlankNodeLabel))
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (runParser)
import Valence.SPARQL.Parser (BlankNode(..), PrefixedName(..), blankNode, prefixedName)

newtype ABlankNode = ABlankNode BlankNode
instance aBlankNode :: Arbitrary ABlankNode where
  arbitrary = do
    i <- chooseInt 0 1 
    case i of 
      0 -> arbitrary >>= (\(ABlankNodeLabel label) -> pure $ ABlankNode (BlankNodeLabel label))
      _ -> pure (ABlankNode Anon)


parserSpec :: ∀ r. Spec (console :: CONSOLE, random :: RANDOM | r) Unit
parserSpec = do 
  describe "Valence.SPARQL.Parser" do
    it "should parse a BlankNode" do
      (runParser blankNode "[]") `shouldEqual` (Right Anon)
      (runParser blankNode "_:asdf") `shouldEqual` (Right (BlankNodeLabel "_:asdf"))
    it "should pass quickCheck" do 
      liftEff' (quickCheck (\(ABlankNodeLabel label) -> (runParser blankNode label) === (Right (BlankNodeLabel label)) ))

    it "should parse a PrefixedName" do 
      (runParser prefixedName "dc:title") `shouldEqual` (Right (PrefixedName { namespace : "dc", local : (Just "title")}))
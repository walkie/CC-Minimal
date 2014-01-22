
module CC.Parser where

import Control.Applicative ((<*))

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec hiding (runParser)
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

import CC.Language
import CC.Object


--
-- * Parser
--

lexer = makeTokenParser emptyDef

parseCC :: (Tag t, Obj e) => Parser (CC t e)
parseCC = try parseChc <|> (parseObj >>= return . Obj)

parseChc :: (Tag t, Obj e) => Parser (CC t e)
parseChc = do
    t <- parseTag <* char '<'
    l <- parseCC  <* char ','
    r <- parseCC  <* char '>'
    return (Chc t l r)

parseOption :: Parser Option
parseOption = identifier lexer

parseSetting :: Parser Setting
parseSetting = do
    o <- parseOption
    char '.'
    b <- try (char 'l' >> return True) <|> 
             (char 'r' >> return False)
    return (o,b)

parseConfig :: Parser Config
parseConfig = fmap Map.fromList (parseSetting `sepBy` char ',')

runParser :: Parser a -> String -> a
runParser p = either (error . show) id . parse (p <* eof) ""

readCC :: (Tag t, Obj e) => String -> CC t e
readCC = runParser parseCC


module CC.Parser where

import Control.Applicative ((<*))

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec hiding (runParser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String
import qualified Text.Parsec.Token as Lex

import CC.Language
import CC.Object
import CC.Tag


--
-- * General Infrastructure
--

-- | Parse a string, returning the value or failing on a parse error.
runParser :: Parser a -> String -> a
runParser p = either (error . show) id . parse (p <* eof) ""

-- | A value that can be parsed from a string.
class Parse a where
  parseIt :: Parser a


--
-- * Lexer
--

lexer = Lex.makeTokenParser (emptyDef {
    Lex.reservedNames = ["True","False"]
  })

parens     = Lex.parens     lexer
comma      = Lex.comma      lexer
reserved   = Lex.reserved   lexer
identifier = Lex.identifier lexer
integer    = Lex.integer    lexer
symbol     = Lex.symbol     lexer


--
-- * Choice Calculus Parser
--

-- | Extend Tag with parse method.
class (Parse t, Tag t) => TagP t where
  parseTag :: Parser t
  parseTag = parseIt

-- | Extend Obj with parse method.
class Obj e => ObjP e where
  parseObj :: Parse a => Parser (e a)

-- | Parse a choice calculus expression.
parseCC :: (TagP t, ObjP e) => Parser (CC t e)
parseCC = try parseChc <|> (parseObj >>= return . Obj)
  where
    parseChc = do
      t <- parseTag <* symbol "<"
      l <- parseCC  <* comma 
      r <- parseCC  <* symbol ">"
      return (Chc t l r)

instance (TagP t, ObjP e) => Parse (CC t e) where
  parseIt = parseCC


--
-- * Other Useful Parsers
--

-- | Parse a configuration option.
parseOption :: Parser Option
parseOption = identifier

-- | Parse a boolean literal (used in formula tags).
parseBool :: Parser Bool
parseBool = (reserved "True"  >> return True) <|>
            (reserved "False" >> return False)

-- | Parse a configuration option setting ("opt.l" or "opt.r").
parseSetting :: Parser Setting
parseSetting = do
    o <- parseOption
    symbol "."
    b <- try ((symbol "l" <|> symbol "L") >> return True) <|>
             ((symbol "r" <|> symbol "R") >> return False)
    return (o,b)

-- | Parse a configuration as a list of settings ("opt1.l,opt2.r").
parseConfig :: Parser Config
parseConfig = fmap Map.fromList (parseSetting `sepBy` comma)

readCC :: (TagP t, ObjP e) => String -> CC t e
readCC = runParser parseCC


--
-- * Tag Parsers
--

parseDim :: Parser Dim
parseDim = fmap Dim parseOption
  
parseFormula :: Parser Formula
parseFormula = buildExpressionParser table terms
  where
    terms = parens parseIt <|> fmap Con parseBool <|> fmap Opt parseOption
    table = [[Prefix (op o Not)           | o <- ["¬","~"]],
             [Infix  (op o And) AssocLeft | o <- ["∧","&"]],
             [Infix  (op o Or ) AssocLeft | o <- ["v","|"]]]
    op o c = symbol o >> return c

instance Parse Dim     where parseIt = parseDim
instance Parse Formula where parseIt = parseFormula


--
-- * Object Language Parsers
--

-- ** Some atomic value types.

parseUnit :: Parser ()
parseUnit = symbol "()" >> return ()

parseInt :: Parser Int
parseInt = fmap fromInteger integer

instance Parse ()  where parseIt = parseUnit
instance Parse Int where parseIt = parseInt

-- ** Object languages

parseNone :: Parser (None e)
parseNone = symbol "_" >> return None

parseOne :: (Show a, Parse a) => Parser (One a e)
parseOne = fmap One parseIt

parseList :: (Show a, Parse a, Parse e) => Parser (List a e)
parseList = (symbol "[]" >> return Nil)
        <|> do h <- parseIt
               symbol ":"
               t <- parseIt
               return (Cons h t)

parseTree :: (Show a, Parse a, Parse e) => Parser (Tree a e)
parseTree = parens (do symbol "Node"
                       a <- parseIt
                       l <- parseIt
                       r <- parseIt
                       return (Node a l r))
        <|> fmap Leaf parseIt

instance ObjP None where
  parseObj = parseNone
instance (Show a, Parse a) => ObjP (One a) where
  parseObj = parseOne
instance (Show a, Parse a) => ObjP (List a) where
  parseObj = parseList
instance (Show a, Parse a) => ObjP (Tree a) where
  parseObj = parseTree

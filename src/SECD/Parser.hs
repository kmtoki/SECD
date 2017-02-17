
module SECD.Parser (
  parse
  ) where

import SECD.Internal

import Control.Applicative
import qualified Text.Parsec as T

parse :: T.SourceName -> String -> Either T.ParseError Lisp
parse = T.parse sexpr

sexpr = spaces *> (atom <|> list) <* spaces

t p = spaces *> p <* spaces

spaces = T.skipMany (T.space <|> T.tab <|> T.newline)

atom = 
  (LNum . read) <$> ((++) <$> T.option "" (T.string "-")  <*> (T.many1 $ T.digit)) <|>
  (T.string "#" *>
    (LFalse <$ T.string "false" <|>
     LTrue <$ T.string "true" <|>
     LNil <$ T.string "nil")) <|>
  LAtom <$> (T.many1 $ T.noneOf "(#)' \t\n")

list = LList <$> (T.string "(" *> T.many sexpr  <* T.string ")")

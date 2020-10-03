module C3PO.Parser
  ( parseLocales
  , module C3PO.Types
  ) where

import           C3PO.Types

import           Data.Bifunctor     (first)
import qualified Data.List.NonEmpty as NE (fromList)
import           Data.Semigroup     (sconcat)
import           Data.String        (IsString (fromString))

import           Text.Parsec
import           Text.Parsec.String (Parser)

parseLocales :: String -> Either String LocalesDefinitions
parseLocales =
  let handle = first show
  in handle . runParser localesDefinitionsParser () "code"

localesDefinitionsParser :: Parser LocalesDefinitions
localesDefinitionsParser = LocalesDefinitions <$> (spaces *> sepEndBy1 localeDefinitionParser endOfLine <* spaces <* eof)

localeDefinitionParser :: Parser MessageDefinition
localeDefinitionParser = spaces *>
  (
    MessageDefinition <$> messageIdParser
                      <*  spaces
                      <*> between quote quote localeParser
                      <*  spaces
                      <*> messageRepresentationParser
  )

messageIdParser :: Parser MessageId
messageIdParser = identifierParser

identifierParser :: IsString str => Parser str
identifierParser =
  let parser =
        (:) <$> oneOf ['a'..'z']
            <*> many (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']))
  in fromString <$> parser

localeParser :: Parser LocaleCode
localeParser = LocaleCode . mconcat <$>
  sequence [ count 2 (oneOf ['a'..'z'])
           , string "_"
           , count 2 (oneOf ['A'..'Z'])
           ]

quote :: Parser Char
quote = char '"'

messageRepresentationParser :: Parser MessageRepresentation
messageRepresentationParser =
  let parser = sconcat . NE.fromList <$> many1 (choices [placeholderParser, literalParser])
  in between quote quote parser

choices :: [Parser a] -> Parser a
choices = choice . fmap try

literalParser :: Parser MessageRepresentation
literalParser = LiteralRepr <$> many1 (choices [ escape *> quote
                                               , escape *> openingBracket
                                               , noneOf "{\""
                                               ]
                                      )

placeholderParser :: Parser MessageRepresentation
placeholderParser =
  PlaceHolderRepr <$> between openingBracket closingBracket identifierParser

escape :: Parser Char
escape = char '\\'

openingBracket :: Parser Char
openingBracket = char '{'

closingBracket :: Parser Char
closingBracket = char '}'

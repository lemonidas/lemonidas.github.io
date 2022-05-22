{-# LANGUAGE InstanceSigs, RankNTypes #-}
module Bibtex where

import Prelude hiding ((<>))
import Control.Applicative hiding (some, many)
import Control.Monad 

import qualified Data.Char as Char

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error

import Text.PrettyPrint (Doc, (<+>), (<>))
import qualified Text.PrettyPrint as PP

import Data.Void (Void)

-- | Data Modelling

-- | A Single Bib Entry
data Bib = Bib
  { _name  :: String
  , _title :: String
  , _procs :: String
  , _short :: String
  , _authors :: String
  , _url  :: String
  , _year :: String
  , _keys :: [String]
  , _rest :: [(String, String)]
  } deriving (Eq, Show)

data TempBib = TempBib
  { _tempName :: String
  , _entries  :: [(String, String)]
  } deriving (Eq, Show)

--makeLenses ''Bib
--makeLenses ''TempBib

-- | Parsec <err> <input>
type Parser = Parsec Void String

inProc :: Parser String
inProc = (string "inproceedings" <|> string "INPROCEEDINGS") <* char '{'

inPHD :: Parser String
inPHD = (string "phdthesis") <* char '{'

inBook :: Parser String
inBook = (string "book") <* char '{'

bibOpen :: Parser String
bibOpen = char '@' *> (inProc <|> inPHD <|> inBook)

sb :: Parser a -> Parser b -> Parser c -> Parser c
sb p1 p2 p = space *> p1 *> space *> p <* space <* p2 <* space

bibOpener :: Parser String
bibOpener = sb bibOpen (char ',') (some alphaNumChar)

entryContent :: Parser String
entryContent = sb (char '{') (char '}') (takeWhileP Nothing (/= '}'))
  <|> takeWhileP Nothing (/= ',')

bibEntry :: Parser (String, String)
bibEntry = (,) <$> many alphaNumChar <*> sb (char '=') (char ',') entryContent

bibEntries :: Parser [(String, String)]
bibEntries = many bibEntry
                                         
tempBibEntry :: Parser TempBib
tempBibEntry = TempBib <$> bibOpener <*> (bibEntries <* char '}' <* space)

--debuger :: Parser (String, String)
debuger = do
  open <- bibOpener
  ea <- many bibEntry
  return (open, ea)

bibs :: Parser [Bib]
bibs = map toBib <$> many tempBibEntry

toBib :: TempBib -> Bib
toBib t@(TempBib tn es) =
  let finder key =
        case lookup key es of
          Just s -> s
          Nothing -> error $ "No " ++ key ++ " in: " ++ show t 
      url  = maybe ("https://lemonidas.github.io/pdf/" ++ tn) id (lookup "url" es)
      keys = maybe [] return (lookup "keys" es)
  in Bib { _name  = tn
         , _title = finder "title"
         , _procs = finder "booktitle"
         , _short = finder "shortbooktitle"
         , _authors = finder "author"
         , _year = finder "year"
         , _url  = url
         , _keys = keys
         , _rest = [] -- TODO: FIX
         }

toHtml :: Bib -> Doc
toHtml bib =
  PP.nest 2 $ PP.vcat [ PP.text "<li>"
                      , PP.nest 2 $ PP.vcat [ PP.text ("<b> " ++ _title bib ++ " </b>")
                                            , PP.text ("<a href=\"./pdf/" ++ _name bib ++ ".pdf\">PDF</a> <a href=\"./bib/" ++ _name bib ++ ".bib\">(BIB)<a/><br/>")
                                            , PP.text ("<p>" ++ _authors bib ++ ". <i> " ++ _short bib ++ " " ++ _year bib ++ "</i></p>")
                                            ]
                      , PP.text ("</li>")
                      ]

pp :: Bib -> Doc
pp bib =
    (PP.text "@inproceedings{" <> PP.text (_name bib)  <> PP.char ',') PP.$$
    PP.nest 2 (PP.vcat [ PP.text "author = {" <> PP.text (_authors bib) <> PP.text "},"
                       , PP.text "title = {" <> PP.text (_title bib) <> PP.text "},"
                       , PP.text "booktitle = {" <> PP.text (_procs bib) <> PP.text "},"
                       , PP.text "shortbooktitle = {" <> PP.text (_short bib) <> PP.text "},"
                       , PP.text "url = {" <> PP.text (_url bib) <> PP.text "},"
                       , PP.text "year = {" <> PP.text (_year bib) <> PP.text "},"
                       ]) PP.$$
    PP.char '}'

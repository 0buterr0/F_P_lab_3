-- | Типи для подання тексту: Символ, Слово, Розділовий знак, Лексема, Речення, Абзац, Текст.
module Types
  ( Symbol (..)
  , Word (..)
  , Punctuation (..)
  , Token (..)
  , Sentence (..)
  , Paragraph (..)
  , Text (..)
  , Render (..)
  , punctuationChar
  ) where

import Prelude hiding (Word)
import Data.List (intercalate)

-- | Символ — один знак тексту.
newtype Symbol = Symbol Char
  deriving (Eq, Show)

-- | Слово — послідовність символів, що починається з букви
--   (усередині можуть бути цифри, '_', апостроф чи дефіс між буквами).
newtype Word = Word [Symbol]
  deriving (Eq, Show)

-- | Розділовий знак.
data Punctuation
  = Period                 -- .
  | Comma                  -- ,
  | Semicolon              -- ;
  | Colon                  -- :
  | Exclamation            -- !
  | Question               -- ?
  | Ellipsis               -- ... або …
  | Dash Char              -- - – —
  | OpenBracket Char       -- ( [ {
  | CloseBracket Char      -- ) ] }
  | QuoteMark Char         -- " « » “ ” „
  | OtherMark Char         -- інші знаки (оператори в прикладах коду тощо)
  deriving (Eq, Show)

-- | Лексема речення.
data Token
  = TWord Word
  | TNumber [Symbol]
  | TPunct Punctuation
  | TSpace                 -- будь-яка послідовність пробілів, табуляцій і переносів рядка
  deriving (Eq, Show)

-- | Речення — послідовність лексем.
newtype Sentence = Sentence [Token]
  deriving (Eq, Show)

-- | Абзац — послідовність речень.
newtype Paragraph = Paragraph [Sentence]
  deriving (Eq, Show)

-- | Текст — послідовність абзаців.
newtype Text = Text [Paragraph]
  deriving (Eq, Show)

punctuationChar :: Punctuation -> String
punctuationChar p = case p of
  Period         -> "."
  Comma          -> ","
  Semicolon      -> ";"
  Colon          -> ":"
  Exclamation    -> "!"
  Question       -> "?"
  Ellipsis       -> "..."
  Dash c         -> [c]
  OpenBracket c  -> [c]
  CloseBracket c -> [c]
  QuoteMark c    -> [c]
  OtherMark c    -> [c]

-- | Перетворення у рядок. Кожна послідовність пробільних символів
--   виводиться одним пробілом.
class Render a where
  render :: a -> String

instance Render Symbol where
  render (Symbol c) = [c]

instance Render Word where
  render (Word syms) = concatMap render syms

instance Render Punctuation where
  render = punctuationChar

instance Render Token where
  render t = case t of
    TWord w    -> render w
    TNumber ds -> concatMap render ds
    TPunct p   -> render p
    TSpace     -> " "

instance Render Sentence where
  render (Sentence ts) = concatMap render ts

instance Render Paragraph where
  render (Paragraph ss) = unwords (map render ss)

instance Render Text where
  render (Text ps) = intercalate "\n\n" (map render ps)

-- | Завдання варіанту 1: сортування слів за зростанням частки голосних букв.
module Analysis
  ( HasLetters (..)
  , isVowel
  , wordsOfText
  , sentencesOfText
  , uniqueWords
  , sortByVowelRatio
  ) where

import Prelude hiding (Word)
import Data.Char (isLetter, toLower)
import Data.List (sortOn)
import Data.Ratio ((%))
import qualified Data.Set as Set

import Types

-- | Голосні букви української та англійської абеток.
isVowel :: Char -> Bool
isVowel c = toLower c `elem` "аеєиіїоуюяaeiouy"

-- | Об'єкти, що містять букви: для них визначено частку голосних.
class HasLetters a where
  lettersOf :: a -> String

  letterCount :: a -> Int
  letterCount = length . lettersOf

  vowelCount :: a -> Int
  vowelCount = length . filter isVowel . lettersOf

  -- | Відношення кількості голосних до загальної кількості букв.
  vowelRatio :: a -> Rational
  vowelRatio x
    | letterCount x == 0 = 0
    | otherwise          = fromIntegral (vowelCount x) % fromIntegral (letterCount x)

instance HasLetters Word where
  lettersOf (Word syms) = [ c | Symbol c <- syms, isLetter c ]

instance HasLetters Sentence where
  lettersOf (Sentence ts) = concat [ lettersOf w | TWord w <- ts ]

sentencesOfText :: Text -> [Sentence]
sentencesOfText (Text ps) = concat [ ss | Paragraph ss <- ps ]

-- | Усі слова тексту в порядку появи.
wordsOfText :: Text -> [Word]
wordsOfText t = [ w | Sentence ts <- sentencesOfText t, TWord w <- ts ]

-- | Слова без повторів (без урахування регістру), перше входження зберігається.
uniqueWords :: [Word] -> [Word]
uniqueWords = go Set.empty
  where
    go :: Set.Set String -> [Word] -> [Word]
    go _ [] = []
    go seen (w : ws)
      | key `Set.member` seen = go seen ws
      | otherwise             = w : go (Set.insert key seen) ws
      where
        key = map toLower (render w)

-- | Сортування за зростанням частки голосних. Сортування стійке:
--   слова з однаковою часткою залишаються в порядку появи в тексті.
sortByVowelRatio :: [Word] -> [Word]
sortByVowelRatio = sortOn vowelRatio

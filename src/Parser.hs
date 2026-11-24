-- | Розбір рядка на абзаци, речення та лексеми.
--   Табуляції, пробіли й переноси рядків усередині абзацу стають однією лексемою TSpace.
module Parser
  ( parseText
  , tokenize
  , splitSentences
  ) where

import Prelude hiding (Word)
import Data.Char (isDigit, isLetter, isSpace)

import Types

-- | Увесь текст: абзаци розділяються порожніми рядками.
parseText :: String -> Text
parseText src =
  Text [ Paragraph ss
       | block <- paragraphBlocks src
       , let ss = splitSentences (tokenize block)
       , not (null ss) ]

-- | Блоки рядків між порожніми рядками.
paragraphBlocks :: String -> [String]
paragraphBlocks = map unlines . splitOnBlank . lines
  where
    blank :: String -> Bool
    blank = all isSpace

    splitOnBlank :: [String] -> [[String]]
    splitOnBlank ls = case dropWhile blank ls of
      []  -> []
      ls' -> let (p, rest) = break blank ls' in p : splitOnBlank rest

---------------------------------------------------------------------
-- Лексичний аналіз
---------------------------------------------------------------------

tokenize :: String -> [Token]
tokenize [] = []
tokenize s@(c : cs)
  | isSpace c  = TSpace : tokenize (dropWhile isSpace cs)
  | isLetter c = let (w, rest) = spanWord s in TWord (Word (map Symbol w)) : tokenize rest
  | isDigit c  = let (n, rest) = spanNumber s in TNumber (map Symbol n) : tokenize rest
  | c == '.' && take 2 cs == ".." = TPunct Ellipsis : tokenize (drop 2 cs)
  | otherwise  = TPunct (classify c) : tokenize cs

-- | Апостроф або дефіс, що може стояти всередині слова (об'єкт, будь-який).
isJoiner :: Char -> Bool
isJoiner x = x `elem` "'\x2019\x02BC-"

spanWord :: String -> (String, String)
spanWord = go []
  where
    go :: String -> String -> (String, String)
    go acc (x : y : rest)
      | isJoiner x && isLetter y = go (y : x : acc) rest
    go acc (x : rest)
      | isLetter x || isDigit x || x == '_' = go (x : acc) rest
    go acc rest = (reverse acc, rest)

-- | Число: цифри, можливо з десятковою крапкою (3.14).
spanNumber :: String -> (String, String)
spanNumber = go []
  where
    go :: String -> String -> (String, String)
    go acc (x : y : rest)
      | x == '.' && isDigit y && not (null acc) = go (y : x : acc) rest
    go acc (x : rest)
      | isDigit x = go (x : acc) rest
    go acc rest = (reverse acc, rest)

classify :: Char -> Punctuation
classify c
  | c == '.'                 = Period
  | c == ','                 = Comma
  | c == ';'                 = Semicolon
  | c == ':'                 = Colon
  | c == '!'                 = Exclamation
  | c == '?'                 = Question
  | c == '\x2026'            = Ellipsis
  | c `elem` "-\x2013\x2014" = Dash c
  | c `elem` "([{"           = OpenBracket c
  | c `elem` ")]}"           = CloseBracket c
  | c `elem` "\"\x00AB\x00BB\x201C\x201D\x201E'" = QuoteMark c
  | otherwise                = OtherMark c

---------------------------------------------------------------------
-- Речення
---------------------------------------------------------------------

isTerminal :: Token -> Bool
isTerminal (TPunct p) = p `elem` [Period, Exclamation, Question, Ellipsis]
isTerminal _          = False

-- | Знаки, що можуть іти одразу за кінцем речення: «?!», «.)», «.»».
isCloser :: Token -> Bool
isCloser t@(TPunct p) = isTerminal t || case p of
  CloseBracket _ -> True
  QuoteMark _    -> True
  _              -> False
isCloser _ = False

-- | Речення закінчується термінальним знаком (. ! ? ...), після якого
--   йде пробіл або кінець абзацу.
splitSentences :: [Token] -> [Sentence]
splitSentences = filter nonEmpty . map trimSentence . go []
  where
    go :: [Token] -> [Token] -> [Sentence]
    go acc [] = [Sentence (reverse acc) | not (null acc)]
    go acc (t : rest)
      | isTerminal t =
          let (closers, rest') = span isCloser rest
              acc'             = reverse closers ++ (t : acc)
          in  case rest' of
                []               -> [Sentence (reverse acc')]
                (TSpace : rest'') -> Sentence (reverse acc') : go [] rest''
                _                -> go acc' rest'
      | otherwise = go (t : acc) rest

    trimSentence :: Sentence -> Sentence
    trimSentence (Sentence ts) =
      Sentence (reverse (dropWhile (== TSpace) (reverse (dropWhile (== TSpace) ts))))

    nonEmpty :: Sentence -> Bool
    nonEmpty (Sentence ts) = not (null ts)

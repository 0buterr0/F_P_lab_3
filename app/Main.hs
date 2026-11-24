{-# LANGUAGE CPP #-}

-- | Лабораторна робота №3, варіант 1.
--   Обробка тексту підручника з програмування: слова сортуються
--   за зростанням частки голосних букв.
module Main (main) where

import Prelude hiding (Word)
import qualified Data.ByteString as BS
import Data.Ratio (denominator, numerator)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Numeric (showFFloat)
import System.Environment (getArgs)
import System.IO

import Analysis
import Parser
import Types

#if defined(mingw32_HOST_OS)
import System.Win32.Console (setConsoleCP, setConsoleOutputCP)
#endif

setupConsole :: IO ()
setupConsole = do
#if defined(mingw32_HOST_OS)
  setConsoleOutputCP 65001
  setConsoleCP 65001
#endif
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]

readUtf8 :: FilePath -> IO String
readUtf8 path = do
  bytes <- BS.readFile path
  let s = T.unpack (TE.decodeUtf8With lenientDecode bytes)
  pure (filter (/= '\xFEFF') s)

writeUtf8 :: FilePath -> String -> IO ()
writeUtf8 path s = BS.writeFile path (TE.encodeUtf8 (T.pack s))

pad :: Int -> String -> String
pad n s = s ++ replicate (n - length s) ' '

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

showRatio :: Rational -> String
showRatio r =
  showFFloat (Just 3) (fromRational r :: Double) ""
  ++ "  (" ++ show (numerator r) ++ "/" ++ show (denominator r) ++ ")"

-- | Таблиця відсортованих слів.
wordTable :: [Word] -> [String]
wordTable ws = header : separator : zipWith line [1 :: Int ..] ws
  where
    wWidth    = maximum (5 : map (length . render) ws) + 2
    header    = padLeft 5 "№" ++ "  " ++ pad wWidth "Слово" ++ pad 10 "Голосних" ++ pad 7 "Букв" ++ "Частка голосних"
    separator = replicate (7 + wWidth + 17 + 22) '-'
    line i w  =
      padLeft 5 (show i) ++ "  " ++ pad wWidth (render w)
      ++ pad 10 (show (vowelCount w)) ++ pad 7 (show (letterCount w)) ++ showRatio (vowelRatio w)

usage :: IO ()
usage = mapM_ putStrLn
  [ "Використання: textproc [файл підручника] [файл результату] [--unique]"
  , "  за замовчуванням: data/textbook.txt -> result.txt"
  , "  --unique  кожне слово один раз (без урахування регістру)"
  ]

main :: IO ()
main = do
  setupConsole
  args <- getArgs
  let unique = "--unique" `elem` args
      files  = filter (/= "--unique") args
  case files of
    ["--help"] -> usage
    _ -> do
      let (input, output) = case files of
            []         -> ("data/textbook.txt", "result.txt")
            [i]        -> (i, "result.txt")
            (i : o : _) -> (i, o)
      src <- readUtf8 input
      let text      = parseText src
          Text ps   = text
          sentences = sentencesOfText text
          allWords  = wordsOfText text
          selected  = if unique then uniqueWords allWords else allWords
          sorted    = sortByVowelRatio selected
          normalized = render text
          report = unlines $
            [ "=== Нормалізований текст (табуляції та послідовності пробілів замінено одним пробілом) ==="
            , ""
            , normalized
            , ""
            , "=== Статистика ==="
            , "Абзаців:   " ++ show (length ps)
            , "Речень:    " ++ show (length sentences)
            , "Слів:      " ++ show (length allWords)
            , "Унікальних слів: " ++ show (length (uniqueWords allWords))
            , ""
            , "=== Слова, відсортовані за зростанням частки голосних букв"
              ++ (if unique then " (без повторів)" else "") ++ " ==="
            , ""
            ] ++ wordTable sorted
      putStr report
      writeUtf8 output report
      putStrLn ""
      putStrLn ("Результат збережено у " ++ output)

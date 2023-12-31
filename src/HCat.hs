module HCat where

import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.Environment qualified as Env
import System.IO.Error qualified as IOError
import System.Info qualified as SystemInfo
import System.Process (readProcess)

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
    "linux" -> tputScreenDimensions
    "darwin" -> tputScreenDimensions
    _ -> return $ ScreenDimensions 25 80
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions =
      readProcess "tput" ["lines"] ""
        >>= \lines ->
          readProcess "tput" ["cols"] ""
            >>= \cols ->
              let lines' = read $ init lines
                  cols' = read $ init cols
               in return $ ScreenDimensions lines' cols'

handleArgs :: IO (Either String FilePath)
handleArgs =
  parseArgs <$> Env.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [fname] -> Right fname
        [] -> Left "no arguments provided"
        _ -> Left "multiple files not supported"

eitherToErr :: (Show a) => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left err) = Exception.throwIO . IOError.userError $ show err

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs =
  let (hd, tl) = splitAt n xs
   in hd : groupsOf n tl

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
      let
        (candidate, nextLines) = Text.splitAt lineLength lineText
        (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
       in
        firstLine : wordWrap lineLength (overflow <> nextLines)
  where
    softWrap hardwrappedText textIndex
      | textIndex <= 0 = (hardwrappedText, Text.empty)
      | Text.index hardwrappedText textIndex == ' ' =
          let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
           in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardwrappedText (textIndex - 1)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  }
  deriving (Show)

paginate :: ScreenDimensions -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows columns) text =
  let
    unwrappedLines = Text.lines text
    wrappedLines = concatMap (wordWrap columns) unwrappedLines
    paginatedLines = groupsOf rows wrappedLines
   in
    map Text.unlines paginatedLines

runHCat :: IO ()
runHCat =
  withErrorHandling $
    handleArgs
      >>= eitherToErr
      >>= TextIO.readFile
      >>= TextIO.putStrLn
  where
    withErrorHandling ioAction =
      Exception.catch ioAction $
        \err -> putStrLn "I ran into an error: " >> print @IOError.IOError err

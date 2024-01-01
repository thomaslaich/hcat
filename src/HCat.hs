module HCat where

import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time.Clock qualified as Clock
import Data.Time.Clock.POSIX qualified as PosixClock
import Data.Time.Format qualified as TimeFormat
import System.Directory qualified as Directory
import System.Environment qualified as Env
import System.IO
import System.IO.Error qualified as IOError
import System.Info qualified as SystemInfo
import System.Process (readProcess)
import Text.Printf

data ContinueCancel = Continue | Cancel deriving (Eq, Show)

data FileInfo = FileInfo
  { filePath :: FilePath
  , fileSize :: Int
  , fileMTime :: Clock.UTCTime
  , fileReadable :: Bool
  , fileWritable :: Bool
  , fileExecutable :: Bool
  }
  deriving (Show)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  }
  deriving (Show)

getFileInfo :: FilePath -> IO FileInfo
getFileInfo filePath = do
  perms <- Directory.getPermissions filePath
  mtime <- Directory.getModificationTime filePath
  size <- BS.length <$> BS.readFile filePath
  return $
    FileInfo
      { filePath = filePath
      , fileSize = size
      , fileMTime = mtime
      , fileReadable = Directory.readable perms
      , fileWritable = Directory.writable perms
      , fileExecutable = Directory.executable perms
      }

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo {..} maxWidth totalPages currentPage =
  let
    timestamp = TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
    permissionString =
      [ if fileReadable then 'r' else '-'
      , if fileWritable then 'w' else '-'
      , if fileExecutable then 'x' else '-'
      ]
    statusLine =
      Text.pack $
        printf
          "%s | permissions: %s | %d bytes | modified: %s | page %d of %d"
          filePath
          permissionString
          fileSize
          timestamp
          currentPage
          totalPages
   in
    invertText (truncateStatus statusLine)
  where
    invertText inputStr =
      let
        reverseVideo = "\^[[7m"
        resetVideo = "\^[[0m"
       in
        reverseVideo <> inputStr <> resetVideo
    truncateStatus statusLine
      | maxWidth <= 3 = ""
      | Text.length statusLine > maxWidth = Text.take (maxWidth - 3) statusLine <> "..."
      | otherwise = statusLine

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

getContinue :: IO ContinueCancel
getContinue = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  input <- hGetChar stdin
  case input of
    ' ' -> return Continue
    'q' -> return Cancel
    _ -> getContinue

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

paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows columns) finfo text =
  let
    rows' = rows - 1
    wrappedLines = concatMap (wordWrap columns) (Text.lines text)
    pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
    pageCount = length pages
    statusLines = map (formatFileInfo finfo columns pageCount) [1 .. pageCount]
   in
    zipWith (<>) pages statusLines
  where
    padTo :: Int -> [Text.Text] -> [Text.Text]
    padTo lineCount rowsToPad =
      take lineCount $ rowsToPad <> repeat ""

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page : pages) = do
  clearScreen
  TextIO.putStrLn page
  input <- getContinue
  case input of
    Continue -> showPages pages
    Cancel -> return ()

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

runHCat :: IO ()
runHCat = do
  targetFilePath <- eitherToErr =<< handleArgs
  contents <- TextIO.hGetContents =<< openFile targetFilePath ReadMode
  termSize <- getTerminalSize
  hSetBuffering stdin NoBuffering
  finfo <- getFileInfo targetFilePath
  let pages = paginate termSize finfo contents
  showPages pages

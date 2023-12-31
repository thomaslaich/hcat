module HCat where

import qualified System.Environment as Env


handleArgs :: IO  (Either String FilePath)
handleArgs = head <$> Env.getArgs

runHCat :: IO ()
runHCat = handleArgs >>= print

module Thicc.ApiKeys (getKeyFor) where
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import System.Directory (doesFileExist)
import System.Entropy
import Thicc.Config
import Thicc.Monad

getKeyFor :: String -> ThiccM (Maybe ApiKey)
getKeyFor f = do
    cfg <- getConfig
    app_exists <- io $ doesFileExist (availableFile cfg f)
    if app_exists
      then Just <$> getKey (apiKeyFile cfg f)
      else return Nothing
  where
    strip = takeWhile (not . isSpace) . dropWhile isSpace
    getKey key_file = do
      key_exists <- io $ doesFileExist key_file
      if key_exists
        then io $ strip <$> readFile key_file
        else generateApiKey key_file

generateApiKey :: FilePath -> ThiccM ApiKey
generateApiKey f = do
  key <- io $ toHex <$> getEntropy 16
  io $ writeFile f key
  return key

toHex :: BS.ByteString -> String
toHex = concatMap hexByte . BS.unpack
  where
    hexByte x =
      let (hi, lo) = fromIntegral x `quotRem` 16
      in [chars !! hi, chars !! lo]
    chars = "0123456789abcdef"
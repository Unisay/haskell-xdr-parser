module Main where

import           Control.Monad.Fail (fail)
import           Data.Xdr.Parser
import           Protolude
import           System.FilePath    (replaceExtension, takeBaseName)
import           Test.Tasty
import           Test.Tasty.Golden
import           Text.Megaparsec
import           Text.Show.Pretty

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  xFiles <- findByExtension [".xdr"] "test/samples"
  pure $ testGroup "Parser golden tests"
    [ goldenVsFile
        (takeBaseName xFile)
        goldenFile
        actualFile
        (parseXdr xFile actualFile)
    | xFile <- xFiles
    , let goldenFile = replaceExtension xFile ".golden.txt"
          actualFile = replaceExtension xFile ".actual.txt"
    ]

parseXdr :: FilePath -> FilePath -> IO ()
parseXdr source target = do
  xdr <- readFile source
  txt <- either (fail . show) (pure . ppShow) $
    runParser specification source xdr
  writeFile target (toS txt)

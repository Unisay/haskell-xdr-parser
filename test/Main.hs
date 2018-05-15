module Main where

import           Control.Monad.Fail (fail)
import           Data.Xdr.Parser
import           Protolude
import           System.FilePath    (replaceExtension, takeBaseName)
import           Test.Tasty
import           Test.Tasty.Golden
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
  res <- parseFile source
  txt <- either (fail . show) (pure . ppShow) res
  writeFile target (toS txt)

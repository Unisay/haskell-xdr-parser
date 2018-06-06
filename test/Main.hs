module Main where

import           Data.Xdr.Parser
import           Protolude
import           System.FilePath   (replaceExtension, takeBaseName)
import           Test.Tasty
import           Test.Tasty.Golden
import           Text.Megaparsec   (parseErrorPretty)
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
  raw <- readFile source
  let par = runStatefulParser specification
      res = runParser par source raw
  txt <- either (pure . parseErrorPretty) (pure . ppShow) res
  writeFile target (toS txt)

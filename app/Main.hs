{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BSL
import           System.Environment (getArgs)
import           System.FilePath
import           System.Directory

import MarkdownToJSON
import MetaModel
import PumlWriter

main' :: FilePath -> FilePath -> IO ()
main' inDir outDir =  do
    createDirectoryIfMissing True outDir
    putStrLn "## start MarkdownToJSON"
    json <- profilesToJSON inDir
    BSL.writeFile (outDir </> "index.json") (encodePretty json)
    putStrLn "## end MarkdownToJSON"
    putStrLn "## start parse JSON"
    case fromJSON json :: (Result Spdx3Model) of 
        Error err -> fail err
        Success a -> do
            putStrLn "## show model"
            writeFile (outDir </> "model.show") (show a)
            putStrLn "## start generate PUML"
            writePumlsToDir outDir a

main :: IO ()
main = getArgs >>= \case
                        [] -> main' "spdx-3-model/model" "gh-pages"
                        [inDir] -> main' inDir (inDir ++ "_out")
                        [inDir, outDir] -> main' inDir outDir
                        _ -> undefined


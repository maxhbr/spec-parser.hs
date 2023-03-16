module Main (main) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BSL

import MarkdownToJSON
import MetaModel
import PumlWriter

main :: IO ()
main = do
    putStrLn "## start MarkdownToJSON"
    json <- profilesToJSON "spdx-3-model/model"
    BSL.writeFile "gh-pages/index.json" (encodePretty json)
    putStrLn "## end MarkdownToJSON"

    putStrLn "## start parse JSON"
    case fromJSON json :: (Result Spdx3Model) of 
        Error err -> fail err
        Success a -> do
            putStrLn "## start generate PUML"
            writePumlsToDir "gh-pages/puml" a


    -- putStrLn "start parsing"
    -- spec <- parseSpecIO "spdx-3-model/model/Core/"
    -- putStrLn "done parsing"

    -- print spec

    -- putStrLn "generate pumls"
    -- specToPuml "gh-pages" spec

    -- putStrLn "generate jsons"




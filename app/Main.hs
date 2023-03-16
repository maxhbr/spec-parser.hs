module Main (main) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BSL

import MarkdownToJSON
import HsDataModel

main :: IO ()
main = do
    putStrLn "## start MarkdownToJSON"
    json <- profilesToJSON "spdx-3-model/model"
    BSL.writeFile "gh-pages/index.json" (encodePretty json)
    putStrLn "## end MarkdownToJSON"

    putStrLn "## start parse JSON"
    case fromJSON json :: (Result Spdx3Model) of 
        Error err -> fail err
        Success a -> undefined


    -- putStrLn "start parsing"
    -- spec <- parseSpecIO "spdx-3-model/model/Core/"
    -- putStrLn "done parsing"

    -- print spec

    -- putStrLn "generate pumls"
    -- specToPuml "gh-pages" spec

    -- putStrLn "generate jsons"




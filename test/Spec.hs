{-# LANGUAGE OverloadedStrings #-}
import Lib
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BSL


main :: IO ()
main = do
    putStrLn "start smoketests"

    parseClassIO "spdx-3-model/model/Core/Classes/Element.md" >>= print

    parseVocabularitiesIO "spdx-3-model/model/Core/Vocabularies" >>= print
    parsePropertiesIO "spdx-3-model/model/Core/Properties" >>= print
    parseClassesIO "spdx-3-model/model/Core/Classes" >>= print

    spec <- parseSpecIO "spdx-3-model/model/Core/"
    print spec

    specToPuml "gh-pages" spec
    BSL.writeFile "gh-pages/index.json" (encodePretty spec)

    putStrLn "done smoketests"

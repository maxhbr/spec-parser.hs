{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
module PumlWriter
    ( writePumlsToDir
    ) where

import MyPrelude
import MetaModel

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import           System.IO
import           System.Process

isBasicType :: Text -> Bool
isBasicType t = T.isPrefixOf "xsd:" t || t == "anyURI"

class Pumlifyable a where
    getPumlFilePath :: a -> FilePath
    default getPumlFilePath :: (BasicSpdx3 a) => a -> FilePath
    getPumlFilePath = (<.> "puml") . T.unpack . name
    writeInnerPumlToH :: Handle -> a -> IO ()
    writePumlToFile :: a -> IO FilePath
    writePumlToFile a = do
        let puml = getPumlFilePath a
        debugM "writePumlToFile" ("writePumlToFile: write " ++ puml ++ " ...")
        -- (createDirectoryIfMissing True . dropFileName) puml

        withFile puml WriteMode $ \h -> do
            hPutStrLn h "@startuml"
            writeInnerPumlToH h a
            hPutStrLn h "@enduml"

        debugM "writePumlToFile" ("writePumlToFile: done write " ++ puml)

        return puml

writeCommentForSummaryAndDescription :: BasicSpdx3 a => Handle -> a -> Text -> IO ()
writeCommentForSummaryAndDescription h a n = do
    T.hPutStrLn h ("note top of " <> n)
    hPutStrLn h "<b>Summary</b>"
    T.hPutStrLn h (summary a)
    hPutStrLn h "<b>Description</b>"
    T.hPutStrLn h (description a)
    T.hPutStrLn h "end note"

instance Pumlifyable Spdx3Vocabulary where
    writeInnerPumlToH h vocabulary@Spdx3Vocabulary{_vocabularyEntries = ves } = do
        let vocabularyName = name vocabulary
        T.hPutStrLn h ("enum " <> vocabularyName <> " {")
        mapM_ (\(k,v) -> T.hPutStrLn h ("    " <> k <> " : " <> v)) (Map.toList ves) 
        hPutStrLn h "}"
        writeCommentForSummaryAndDescription h vocabulary vocabularyName

instance Pumlifyable Spdx3Class where
    writeInnerPumlToH h cls@Spdx3Class{_classProperties = props} = do
        let className = name cls
        if cls `metadata` "Instantiability" == Just "Abstract"
            then T.hPutStrLn h ("abstract " <> className <> " {")
            else T.hPutStrLn h ("class " <> className <> " {")
        hPutStrLn h ".. metadata .."
        mapM_ (\(k,v) -> T.hPutStrLn h ("    " <> k <> " : " <> v)) (Map.toList (rawMetadata cls)) 
        hPutStrLn h ".. properties .."
        mapM_ (\(k,Spdx3ClassPropertyParameters ty minCount maxCount) -> let
                numToStr :: Maybe Int -> Text
                numToStr (Just i) = T.pack (show i)
                numToStr Nothing = ""
                numsToRange Nothing Nothing = ""
                numsToRange low up = "[" <> numToStr low <> ".." <> numToStr up <> "]"
            in do
                T.hPutStrLn h ("    " <> T.unwords [k, ":", ty, numsToRange minCount maxCount])
            ) (Map.toList props) 
        hPutStrLn h "}"
        writeCommentForSummaryAndDescription h cls className

        case cls `metadata` "SubclassOf" of
            Just sco ->
                unless (sco == "none" || isBasicType sco) $
                    T.hPutStrLn h ("\"" <> sco <> "\" <|-[thickness=4]- \"" <> className <> "\"")
            Nothing -> pure ()

        mapM_ (\(k,Spdx3ClassPropertyParameters ty _ _) ->
            unless (isBasicType ty) $
                T.hPutStrLn h ("\"" <> ty <> "\" <--- \"" <> className <> "::" <> k <> "\"")
                ) (Map.toList props)

instance Pumlifyable Spdx3Profile where
    writeInnerPumlToH h profile@Spdx3Profile{ _profileProperties = pps
                                            , _profileVocabularies = pvs
                                            , _profileClasses = pcs
        } = do
            hPutStrLn h "' vocabulary"
            mapM_ (\(vocabularyName,vocabulary) -> do
                puml <- writePumlToFile vocabulary
                hPutStrLn h ("!include_once " ++ puml)
                ) (Map.assocs pvs)
            hPutStrLn h "' classes"
            mapM_ (\(className,cls) -> do
                puml <- writePumlToFile cls
                hPutStrLn h ("!include_once " ++ puml)
                ) (Map.assocs pcs)

instance Pumlifyable Spdx3Model where
    getPumlFilePath _ = "spdx3.puml"
    writeInnerPumlToH h (Spdx3Model profiles) = do
        mapM_ (\(profileName,profile) -> do
            T.hPutStrLn h ("package " <> profileName <> " {")
            profilePuml <- writePumlToFile profile
            hPutStrLn h ("!include_once " ++ profilePuml)
            T.hPutStrLn h "}"
            writeCommentForSummaryAndDescription h profile profileName
            ) (Map.assocs profiles)

writePumlsToDir :: FilePath -> Spdx3Model -> IO ()
writePumlsToDir outDir model = do
    createDirectoryIfMissing True outDir
    setCurrentDirectory outDir
    puml <- writePumlToFile model
    debugM "writePumlsToDir" "writePumlsToDir: render svg ..."
    callProcess "plantuml" ["-tsvg", puml] 
    debugM "writePumlsToDir" "writePumlsToDir: done"

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
    getKind :: a -> String
    getPumlFilePath :: a -> FilePath
    default getPumlFilePath :: (BasicSpdx3 a) => a -> FilePath
    getPumlFilePath a = ((<.> getKind a <.> "puml") . T.unpack . name) a
    writeInnerPumlToH :: Handle -> a -> IO [Text]
    writePumlToFile :: a -> IO FilePath
    writePumlToFile a = do
        let puml = getPumlFilePath a
        debugM "writePumlToFile" ("writePumlToFile: write " ++ puml ++ " ...")
        -- (createDirectoryIfMissing True . dropFileName) puml

        withFile puml WriteMode $ \h -> do
            hPutStrLn h "@startuml"
            additionalLines <- writeInnerPumlToH h a
            mapM_ (T.hPutStrLn h) (nub additionalLines)
            hPutStrLn h "@enduml"

        debugM "writePumlToFile" ("writePumlToFile: done write " ++ puml)

        return puml

writeCommentTextForSummaryAndDescription :: BasicSpdx3 a => Handle -> a -> Text -> IO ()
writeCommentTextForSummaryAndDescription h a n = do
    hPutStrLn h "<b>Summary</b>"
    T.hPutStrLn h (summary a)
    hPutStrLn h "<b>Description</b>"
    T.hPutStrLn h (description a)

writeCommentForSummaryAndDescription :: BasicSpdx3 a => Handle -> a -> Text -> IO ()
writeCommentForSummaryAndDescription h a n = do
    T.hPutStrLn h ("note top of " <> n)
    writeCommentTextForSummaryAndDescription h a n
    T.hPutStrLn h "end note"

instance Pumlifyable Spdx3Vocabulary where
    getKind _ = "Vocabulary"
    writeInnerPumlToH h vocabulary@Spdx3Vocabulary{_vocabularyEntries = ves } = do
        let vocabularyName = name vocabulary
        T.hPutStrLn h ("enum " <> vocabularyName <> " {")
        writeCommentTextForSummaryAndDescription h vocabulary vocabularyName
        hPutStrLn h ".. entries .."
        mapM_ (\(k,v) -> T.hPutStrLn h ("    " <> k <> " : " <> v)) (Map.toList ves) 
        hPutStrLn h "}"

        return []

instance Pumlifyable Spdx3Class where
    getKind _ = "Class"
    writeInnerPumlToH h cls@Spdx3Class{_classProperties = props} = do
        let className = name cls
        if cls `metadata` "Instantiability" == Just "Abstract"
            then T.hPutStrLn h ("abstract " <> className <> " {")
            else T.hPutStrLn h ("class " <> className <> " {")
        writeCommentTextForSummaryAndDescription h cls className
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

        let additionalLines = case cls `metadata` "SubclassOf" of
                Just sco -> if sco == "none" || isBasicType sco
                            then []
                            else case T.splitOn ":" sco of
                                [ns, sco'] -> ["\"" <> sco' <> "\" <|-[thickness=4]--- \"" <> className <> "\" : " <> sco]
                                _ -> ["\"" <> sco <> "\" <|-[thickness=4]- \"" <> className <> "\""]
                Nothing -> []

        mapM_ (\(k,Spdx3ClassPropertyParameters ty _ _) ->
            unless (isBasicType ty) $
                T.hPutStrLn h ("\"" <> ty <> "\" <-[dashed,thickness=4]-- \"" <> className <> "::" <> k <> "\"")
                ) (Map.toList props)

        return additionalLines

instance Pumlifyable Spdx3Profile where
    getKind _ = "Profile"
    writeInnerPumlToH h profile@Spdx3Profile{ _profileProperties = pps
                                            , _profileVocabularies = pvs
                                            , _profileClasses = pcs
        } = do
            hPutStrLn h "' vocabulary"
            fromPvs <- mapM (\(vocabularyName,vocabulary) -> do
                writeInnerPumlToH h vocabulary
                ) (Map.assocs pvs)
            hPutStrLn h "' classes"
            T.hPutStrLn h "together {"
            fromPcs <- mapM (\(className,cls) -> do
                writeInnerPumlToH h cls
                ) (Map.assocs pcs)
            T.hPutStrLn h "}"

            return (concat $ fromPvs ++ fromPcs)

instance Pumlifyable Spdx3Model where
    getKind _ = "Model"
    getPumlFilePath a = getKind a <.> "puml"
    writeInnerPumlToH h (Spdx3Model profiles) = do
        concat <$> mapM (\(profileName,profile) -> do
            -- T.hPutStrLn h "together {"
            -- T.hPutStrLn h ("package " <> profileName <> " {")
            additionalLines <- writeInnerPumlToH h profile
            -- T.hPutStrLn h "}"
            -- writeCommentForSummaryAndDescription h profile profileName
            return additionalLines
            ) (Map.assocs profiles)

writePumlsToDir :: FilePath -> Spdx3Model -> IO ()
writePumlsToDir outDir model = do
    createDirectoryIfMissing True outDir
    setCurrentDirectory outDir
    puml <- writePumlToFile model
    debugM "writePumlsToDir" "writePumlsToDir: render svg ..."
    callProcess "plantuml" ["-tsvg", puml] 
    debugM "writePumlsToDir" "writePumlsToDir: done"

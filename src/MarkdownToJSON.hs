{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module MarkdownToJSON
    ( profilesToJSON
    )
    where

import MyPrelude

import qualified Text.Pandoc as P hiding (glob)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import           GHC.Generics
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import           Data.Bifunctor (second)

blocksToTextM :: P.PandocMonad m => [P.Block] -> m Text
blocksToTextM blocks = let
        pandoc = P.Pandoc mempty blocks
    in T.strip <$> P.writeMarkdown P.def pandoc
inlinesToTextM :: P.PandocMonad m => [P.Inline] -> m Text
inlinesToTextM = blocksToTextM . (:[]) . P.Plain

blocksToText :: [P.Block] -> Text
blocksToText = fromRight undefined . P.runPure . blocksToTextM
inlinesToText :: [P.Inline] -> Text
inlinesToText = fromRight undefined . P.runPure . inlinesToTextM

type Cluster a = [(Maybe Text,a)]
clusterBlocksByHeader :: Int -> [P.Block] -> Cluster [P.Block]
clusterBlocksByHeader expectedLevel = let
        clusterBlocksByHeader' :: [(Maybe Text,[P.Block])] -> [P.Block] -> [(Maybe Text,[P.Block])]
        clusterBlocksByHeader' [] blocks = clusterBlocksByHeader' [(Nothing, [])] blocks
        clusterBlocksByHeader' accum [] = accum
        clusterBlocksByHeader' accum@((h,accumBlocks):accumRest) (block@(P.Header thisLevel _ headerInlines):blocks) = 
            if expectedLevel == thisLevel
                then clusterBlocksByHeader' ((Just (inlinesToText headerInlines),[]):accum) blocks
                else clusterBlocksByHeader' ((h,accumBlocks ++ [block]):accumRest) blocks
        clusterBlocksByHeader' ((h,accumBlocks):accumRest) (block:blocks) = clusterBlocksByHeader' ((h,accumBlocks ++ [block]):accumRest) blocks
    in reverse . clusterBlocksByHeader' []

pandocToClusters :: P.Pandoc -> Cluster (Cluster [P.Block])
pandocToClusters raw@(P.Pandoc _ arr) = (map (second (clusterBlocksByHeader 2)) . clusterBlocksByHeader 1) arr

uncluster :: Cluster Value -> Value
uncluster = Object . foldl (\acc (k,v) -> KM.insert (K.fromText $ fromMaybe "_" k) v acc) mempty . filter (/= (Nothing,""))

listToJSON' :: [P.Block] -> Object
listToJSON' [] = mempty
listToJSON' (P.BulletList bullets:otherBulletLists) = let
        fun m [P.Plain ((P.Str key):content)] = 
            KM.insert (K.fromText . fromMaybe key $ T.stripSuffix ":" key) (String $ inlinesToText content) m
        fun m bs = trace (show bs) undefined
    in foldl fun (listToJSON' otherBulletLists) bullets
listToJSON :: [P.Block] -> Value
listToJSON = Object . listToJSON'

listofListsToJSON':: [P.Block] -> Object
listofListsToJSON' [] = mempty
listofListsToJSON' (P.BulletList bullets:otherBulletLists) = let
        fun m (key:value) = KM.insert (K.fromText $ blocksToText [key]) (listToJSON value) m
        fun m bs = trace (show bs) undefined
    in foldl fun (listofListsToJSON' otherBulletLists) bullets
listofListsToJSON :: [P.Block] -> Value
listofListsToJSON = Object . listofListsToJSON'

clusterToJSON :: Maybe Text -> [P.Block] -> Value
clusterToJSON Nothing             = String . blocksToText
clusterToJSON (Just "Metadata")   = listToJSON
clusterToJSON (Just "Entries")    = listToJSON
clusterToJSON (Just "Properties") = listofListsToJSON
clusterToJSON (Just _)            = String . blocksToText
clustersToJSON :: Cluster (Cluster [P.Block]) -> Value
clustersToJSON = uncluster . map (second (uncluster . map (\(k,v) -> (k, clusterToJSON k v))))


lowerClusterOne :: Cluster (Cluster a) -> (Text, Cluster a)
lowerClusterOne c = case filter (\(k,_) -> isJust k ) c of
    [(Just k, v)] -> (k,v)
    _ -> undefined
lowerAndUncluster :: Cluster (Cluster [P.Block]) -> (Text, Value)
lowerAndUncluster cc = let
        (name,c) = lowerClusterOne cc
        Object o = (uncluster . map (\(k,v) -> (k, clusterToJSON k v))) c 
    in (name, Object (KM.insert (K.fromText "_title") (String name) o))


markdownToJSON :: FilePath -> IO (Text, Value)
markdownToJSON md = do
    debugM "markdownToJSON" ("markdownToJSON: parse " ++ md)
    bs <- BS.readFile md
    case T.decodeUtf8' bs of
        Right text -> do
            case P.runPure $ P.readMarkdown P.def text of
                Right p ->
                    (return . lowerAndUncluster . pandocToClusters) p
                Left err -> undefined
        Left err -> undefined

directoryToJSON :: FilePath -> IO Value
directoryToJSON dir = do
    debugM "directoryToJSON" ("directoryToJSON: parse " ++ dir)
    mds <- filter (not . ("/_" `isInfixOf`)) <$> glob (dir </> "*.md")
    jsons <- mapM (\md -> do
        let fromBaseName = takeBaseName md
        (fromHeader, json) <- markdownToJSON md
        return $ if T.pack fromBaseName == fromHeader
            then (Just fromHeader, json)
            else trace (T.unpack (T.pack fromBaseName <> " /= " <> fromHeader)) undefined
        ) mds
    return (uncluster jsons)

profileToJSON :: FilePath -> IO Value
profileToJSON dir = do
    debugM "profileToJSON" ("profileToJSON: parse " ++ dir)
    properties <- directoryToJSON (dir </> "Properties")
    vocabularies <- directoryToJSON (dir </> "Vocabularies")
    classes <- directoryToJSON (dir </> "Classes")
    let name = takeBaseName dir
    (fromHeader, Object o) <- markdownToJSON (dir </> name <.> "md")
    return . Object . KM.insert "Properties" properties . KM.insert "Vocabularies" vocabularies . KM.insert "Classes" classes $ o

profilesToJSON :: FilePath -> IO Value
profilesToJSON dir = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    debugM "profilesToJSON" ("profilesToJSON: parse " ++ dir)
    
    profiles <- filter (not . (".md" `isSuffixOf`)) <$> glob (dir </> "*")
    jsons <- mapM (\profile -> do
        let name = takeBaseName profile
        json <- profileToJSON profile
        return (Just (T.pack name), json)
        ) profiles
    return (uncluster jsons)

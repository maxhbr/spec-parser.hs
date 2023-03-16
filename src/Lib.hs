{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    where

import           Text.Pandoc as P hiding (glob)
-- import qualified Text.Pandoc.Options as P
-- import qualified Text.Pandoc.Readers.Markdown as P

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import           Data.Maybe (fromMaybe, fromJust)
import           System.FilePath
import           System.Directory
import           System.FilePath.Glob (glob)
import           Data.List (isInfixOf)
import           System.IO
import           System.Process
import           Control.Monad
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Encode.Pretty

data SpecRaw
    = SpecRaw
    { header :: Maybe T.Text
    , start :: [Block]
    , sections :: Map.Map Text [Block]
    , raw :: P.Pandoc
    } deriving (Show)

blocksToText :: PandocMonad m => [P.Block] -> m Text
blocksToText blocks = let
        pandoc = Pandoc mempty blocks
    in T.strip <$> P.writeMarkdown def pandoc
inlinesToText :: PandocMonad m => [P.Inline] -> m Text
inlinesToText = blocksToText . (:[]) . P.Plain

-- Pandoc
--    (Meta {unMeta = fromList []}) 
--    [ Para [Str "SPDX-License-Identifier:",Space,Str "Community-Spec-1.0"]
--    , Header 1 ("",[],[]) [Str "AnnotationType"]
--    , Header 2 ("",[],[]) [Str "Summary"]
--    , Para [Str "TODO"]
--    , Header 2 ("",[],[]) [Str "Description"]
--    , Para [Str "TODO"]
--    , Header 2 ("",[],[]) [Str "Metadata"]
--    , BulletList [[Plain [Str "name:",Space,Str "AnnotationType"]]]
--    , Header 2 ("",[],[]) [Str "Entries"]
--    , BulletList [[Plain [Str "other:",Space,Str "TODOdescription"]]
--                 ,[Plain [Str "review:",Space,Str "TODOdescription"]]
--                 ]
--    ]
clusterPandoc :: P.Pandoc -> PandocIO SpecRaw
clusterPandoc raw@(P.Pandoc _ arr) = let
        clusterPandoc' :: Maybe Text -> [Block] -> SpecRaw -> PandocIO SpecRaw
        clusterPandoc' _ [] previous = return previous
        clusterPandoc' _ ((Header 2 _ [P.Str header]):rest) previous = do
            trace ("found h2: " <> header)
            clusterPandoc' (Just header) rest previous 
        clusterPandoc' Nothing ((Header 1 _ header):rest) previous = do
            headerText <- inlinesToText header
            trace ("found h1: " <> headerText)
            clusterPandoc' Nothing rest (previous {header = Just headerText})
        clusterPandoc' Nothing (block:rest) previous@(SpecRaw {start = s}) = let
                next = previous{start = s ++ [block] }
            in clusterPandoc' Nothing rest next
        clusterPandoc' (Just currentSection) (block:rest) previous@(SpecRaw {sections = previousSections}) = let
                next = previous{sections = Map.insertWith (++) currentSection [block] previousSections}
            in clusterPandoc' (Just currentSection) rest next
    in clusterPandoc' Nothing arr (SpecRaw Nothing mempty mempty raw)

readToRaw :: FilePath -> PandocIO SpecRaw
readToRaw md = do
        setTrace True
        bs <- readFileStrict md
        case T.decodeUtf8' bs of
            Right text -> do
                p <- P.readMarkdown P.def text
                trace (T.pack $ show p)
                clusterPandoc p
            Left err -> undefined

-- #######################################################################################
-- #######################################################################################
-- #######################################################################################

data SpecCommon a
    = SpecCommon
    { commonName :: Text
    , commonStart :: Text
    , commonSummary :: Text
    , commonDescription :: Text
    , commonMetadata :: Map.Map Text Text
    , custom :: a
    } deriving (Show, Generic)
instance (ToJSON a) => ToJSON (SpecCommon a) where
    toEncoding = genericToEncoding defaultOptions

parseMap :: ([Inline] -> [Block] -> a) -> [Block] -> Map.Map Text a
parseMap toValue [] = mempty
parseMap toValue (P.BulletList bullets:otherBulletLists) = let
        fun m ((P.Plain inlines):blocks) = 
            case inlines of 
                ((P.Str key):content) -> Map.insert (fromMaybe key $ T.stripSuffix ":" key) (toValue content blocks) m
                _ -> undefined
        fun m _ = undefined
    in foldl fun (parseMap toValue otherBulletLists) bullets
parseMap _ _ = undefined
defaultParseMapFun :: [Inline] -> [Block] -> Text
defaultParseMapFun inlines blocks = case P.runPure (blocksToText (P.Plain inlines: blocks)) of
                                                        Right contentText -> contentText
                                                        _ -> undefined
parseCommon :: a -> SpecRaw -> PandocIO (SpecCommon a)
parseCommon a raw = do
    let cName = fromMaybe "" (header raw)
    cStart <- blocksToText (start raw)
    cSummary <- blocksToText (Map.findWithDefault [] "Summary" (sections raw))
    cDescription <- blocksToText (Map.findWithDefault [] "Description" (sections raw))
    let cMetadata = parseMap defaultParseMapFun (Map.findWithDefault [] "Metadata" (sections raw))
    return $ SpecCommon cName cStart cSummary cDescription cMetadata a

-- #######################################################################################
-- #######################################################################################
-- #######################################################################################

type SpecVocabulary = SpecCommon (Map.Map Text Text)
parseVocabulary :: FilePath -> PandocIO SpecVocabulary
parseVocabulary md = do
    let nameFromFile = takeBaseName md
    trace (T.pack $ "parse " <> md <> " for " <> nameFromFile)
    raw <- readToRaw md
    let custom = parseMap defaultParseMapFun (Map.findWithDefault [] "Entries" (sections raw))
    parseCommon custom raw
parseVocabularities :: [FilePath] -> PandocIO [SpecVocabulary]
parseVocabularities = mapM parseVocabulary

type SpecProperty = SpecCommon ()
parseProperty :: FilePath -> PandocIO SpecProperty
parseProperty md = do
    raw <- readToRaw md
    parseCommon () raw
parseProperties :: [FilePath] -> PandocIO [SpecProperty]
parseProperties = mapM parseProperty

{-
   Pandoc (Meta {unMeta = fromList []})
       [Para [Str "SPDX-License-Identifier:",Space,Str "Community-Spec-1.0"]
       ,Header 1 ("",[],[]) [Str "Element"]
       ,Header 2 ("",[],[]) [Str "Summary"]
       ,Para [Str "Base",Space,Str "domain",Space,Str "class",Space,Str "from",Space,Str "which",Space,Str "all",Space,Str "other",Space,Str "SPDX-3.0",Space,Str "domain",Space,Str "classes",Space,Str "derive."]
       ,Header 2 ("",[],[]) [Str "Description"]
       ,Para [Str "An",Space,Str "Element",Space,Str "is",Space,Str "a",Space,Str "representation",Space,Str "of",Space,Str "a",Space,Str "fundamental",Space,Str "concept",Space,Str "either",Space,Str "directly",Space,Str "inherent",SoftBreak,Str "to",Space,Str "the",Space,Str "Bill",Space,Str "of",Space,Str "Materials",Space,Str "(BOM)",Space,Str "domain",Space,Str "or",Space,Str "indirectly",Space,Str "related",Space,Str "to",Space,Str "the",Space,Str "BOM",Space,Str "domain",SoftBreak,Str "and",Space,Str "necessary",Space,Str "for",Space,Str "contextually",Space,Str "characterizing",Space,Str "BOM",Space,Str "concepts",Space,Str "and",Space,Str "relationships.",SoftBreak,Str "Within",Space,Str "SPDX-3.0",Space,Str "structure",Space,Str "this",Space,Str "is",Space,Str "the",Space,Str "base",Space,Str "class",Space,Str "acting",Space,Str "as",Space,Str "a",Space,Str "consistent,",SoftBreak,Str "unifying,",Space,Str "and",Space,Str "interoperable",Space,Str "foundation",Space,Str "for",Space,Str "all",Space,Str "explicit",SoftBreak,Str "and",Space,Str "inter-relatable",Space,Str "content",Space,Str "objects."]
       ,Header 2 ("",[],[]) [Str "Metadata"]
       ,BulletList [[Plain [Str "name:",Space,Str "Element"]]
                   ,[Plain [Str "SubclassOf:",Space,Str "Payload"]]
                   ,[Plain [Str "Instantiability:",Space,Str "Abstract"]]]
       ,Header 2 ("",[],[]) [Str "Properties"]
       ,BulletList [[Plain [Str "spdxId"]
                    ,BulletList [[Plain [Str "type:",Space,Str "anyURI"]]
                                ,[Plain [Str "minCount:",Space,Str "1"]]
                                ,[Plain [Str "maxCount:",Space,Str "1"]]
                                ]
                    ]
                   ,[Plain [Str "name"]
                    ,BulletList [[Plain [Str "type:",Space,Str "xsd:string"]]
                                ,[Plain [Str "maxCount:",Space,Str "1"]]
                                ]
                    ]
                   ,[Plain [Str "summary"],BulletList [[Plain [Str "type:",Space,Str "xsd:string"]],[Plain [Str "maxCount:",Space,Str "1"]]]],[Plain [Str "description"],BulletList [[Plain [Str "type:",Space,Str "xsd:string"]],[Plain [Str "maxCount:",Space,Str "1"]]]],[Plain [Str "comment"],BulletList [[Plain [Str "type:",Space,Str "xsd:string"]],[Plain [Str "maxCount:",Space,Str "1"]]]],[Plain [Str "creationInfo"],BulletList [[Plain [Str "type:",Space,Str "CreationInformation"]],[Plain [Str "minCount:",Space,Str "1"]],[Plain [Str "maxCount:",Space,Str "1"]]]],[Plain [Str "verifiedUsing"],BulletList [[Plain [Str "type:",Space,Str "IntegrityMethod"]]]],[Plain [Str "externalReferences"],BulletList [[Plain [Str "type:",Space,Str "ExternalReference"]]]],[Plain [Str "externalIdentifiers"],BulletList [[Plain [Str "type:",Space,Str "ExternalIdentifier"]]]],[Plain [Str "extensions"],BulletList [[Plain [Str "type:",Space,Str "Extension"]]]]]]
 -}
type SpecClass = SpecCommon (Map.Map Text (Map.Map Text Text))
parseClass :: FilePath -> PandocIO SpecClass
parseClass md = do
    raw <- readToRaw md
    let custom = parseMap (\inlines blocks -> case inlines of 
                                                    [] -> parseMap defaultParseMapFun blocks
                                                    _ -> undefined) (Map.findWithDefault [] "Properties" (sections raw))
    parseCommon custom raw
parseClasses :: [FilePath] -> PandocIO [SpecClass]
parseClasses = mapM parseClass

data Spec
    = Spec
    { vocabularies :: Map.Map Text SpecVocabulary
    , properties :: Map.Map Text SpecProperty
    , classes :: Map.Map Text SpecClass
    } deriving (Show, Generic)
instance ToJSON Spec where
    toEncoding = genericToEncoding defaultOptions
parseSpec ::[FilePath] -> [FilePath] -> [FilePath] -> PandocIO Spec
parseSpec vocMDs propMDs clsMDs = let
        mappify :: [SpecCommon a] -> Map.Map Text (SpecCommon a)
        mappify = let
                fun acc c@SpecCommon {commonName = n, commonMetadata = cm} = let
                        name = Map.findWithDefault n "name" cm
                    in (name,c): acc
            in Map.fromList . foldl fun []
    in do
        vocs <- mappify <$> parseVocabularities vocMDs
        props <- mappify <$> parseProperties propMDs
        clses <- mappify <$> parseClasses clsMDs
        return $ Spec vocs props clses

-- #######################################################################################
-- #######################################################################################
-- #######################################################################################

listMDs :: FilePath -> IO [FilePath]
listMDs dir = filter (not . ("/_" `isInfixOf`)) <$> glob (dir </> "*.md")

parseVocabularyIO :: FilePath -> IO SpecVocabulary
parseVocabularyIO = P.runIOorExplode . parseVocabulary
parseVocabularitiesIO :: FilePath -> IO [SpecVocabulary]
parseVocabularitiesIO dir = do
    mds <- listMDs dir
    P.runIOorExplode $ parseVocabularities mds

parsePropertyIO :: FilePath -> IO SpecProperty
parsePropertyIO = P.runIOorExplode . parseProperty
parsePropertiesIO :: FilePath -> IO [SpecProperty]
parsePropertiesIO dir = do
    mds <- listMDs dir
    P.runIOorExplode $ parseProperties mds

parseClassIO :: FilePath -> IO SpecClass
parseClassIO = P.runIOorExplode . parseClass
parseClassesIO :: FilePath -> IO [SpecClass]
parseClassesIO dir = do
    mds <- listMDs dir
    P.runIOorExplode $ parseClasses mds

parseSpecIO :: FilePath -> IO Spec
parseSpecIO root = do
    vocMDs <- listMDs (root </> "Vocabularies")
    propMDs <- listMDs (root </> "Properties")
    clsMDs <- listMDs (root </> "Classes")
    P.runIOorExplode $ parseSpec vocMDs propMDs clsMDs

-- #######################################################################################
-- #######################################################################################
-- #######################################################################################

isBasicType :: Text -> Bool
isBasicType t = T.isPrefixOf "xsd:" t || t == "anyURI"

nameToIdentifier :: Text -> Text
nameToIdentifier = T.replace ":" "_"

vocabularyToPuml :: FilePath -> Spec -> Text -> IO FilePath
vocabularyToPuml root context@(Spec{vocabularies = vocabularies}) key = let
        relativePath = "Vocabularies" </> T.unpack key <.> "puml"
        out = root </> relativePath
        outCommented = out -<.> "commented.puml"
    in do
        (createDirectoryIfMissing True . dropFileName) out

        withFile out WriteMode $ \h -> do
            hPutStrLn h "@startuml"
            case Map.lookup key vocabularies of
                Nothing -> hPutStrLn h "note: failed to find vocabulary"
                Just (SpecCommon
                        { commonName = commonName
                        , commonStart = commonStart
                        , commonSummary = commonSummary
                        , commonDescription = commonDescription
                        , commonMetadata = commonMetadata
                        , custom = custom
                }) -> do
                    T.hPutStrLn h ("enum " <> commonName <> " {")
                    mapM_ (\(k,v) -> T.hPutStrLn h ("    " <> k <> " : " <> v)) (Map.toList custom) 
                    hPutStrLn h "}"
                    T.hPutStrLn h ("note top of " <> commonName)
                    T.hPutStrLn h (T.unlines ["<b>Summary</b>", commonSummary, "<b>Description</b>", commonDescription])
                    T.hPutStrLn h "end note"
            hPutStrLn h "@enduml"
        callProcess "plantuml" ["-tsvg", out] 

        withFile outCommented WriteMode $ \h -> do
            hPutStrLn h "@startuml"
            case Map.lookup key vocabularies of
                Nothing -> hPutStrLn h "note: failed to find vocabulary"
                Just (SpecCommon
                        { commonName = commonName
                        , commonStart = commonStart
                        , commonSummary = commonSummary
                        , commonDescription = commonDescription
                        , commonMetadata = commonMetadata
                        , custom = custom
                }) -> do
                    hPutStrLn h ("!include_once ../" ++ relativePath)
                    mapM_ (\(k,v) -> do
                            T.hPutStrLn h ("note right of " <> commonName <> "::" <> k)
                            T.hPutStrLn h ("    " <> v)
                            T.hPutStrLn h "end note"
                        ) (Map.toList custom) 
            hPutStrLn h "@enduml"
        callProcess "plantuml" ["-tsvg", outCommented]

        return relativePath

vocabulariesToPuml :: FilePath -> Spec -> IO FilePath
vocabulariesToPuml root context@(Spec{vocabularies = vocabularies}) = let
        relativePath = "Vocabularies" </> "index" <.> "puml"
        out = root </> relativePath
        outCommented = out -<.> "commented.puml"
    in do
        vPumls <- mapM (vocabularyToPuml root context) (Map.keys vocabularies)

        withFile out WriteMode $ \h -> do
            hPutStrLn h "@startuml"
            mapM_ (\puml -> hPutStrLn h ("!include_once ../" ++ puml)) vPumls
            hPutStrLn h "@enduml"
        callProcess "plantuml" ["-tsvg", out] 

        withFile outCommented WriteMode $ \h -> do
            hPutStrLn h "@startuml"
            mapM_ (\puml -> hPutStrLn h ("!include_once ../" ++ (puml -<.> "commented.puml"))) vPumls
            hPutStrLn h "@enduml"
        callProcess "plantuml" ["-tsvg", outCommented] 

        return relativePath

classToPuml :: FilePath -> Spec -> Text -> IO FilePath
classToPuml root context@(Spec{classes = classes}) key = let
        relativePath = "Classes" </> T.unpack key <.> "puml"
        out = root </> relativePath
        outCommented = out -<.> "commented.puml"
    in do
        (createDirectoryIfMissing True . dropFileName) out

        withFile out WriteMode $ \h -> do
            hPutStrLn h "@startuml"
            case Map.lookup key classes of
                Nothing -> hPutStrLn h "note: failed to find class"
                Just (SpecCommon
                        { commonName = commonName
                        , commonStart = commonStart
                        , commonSummary = commonSummary
                        , commonDescription = commonDescription
                        , commonMetadata = commonMetadata
                        , custom = custom
                }) -> do
                    if Map.lookup "Instantiability" commonMetadata == Just "Abstract"
                        then T.hPutStrLn h ("abstract " <> commonName <> " {")
                        else T.hPutStrLn h ("class " <> commonName <> " {")
                    hPutStrLn h ".. metadata .."
                    mapM_ (\(k,v) -> T.hPutStrLn h ("    " <> k <> " : " <> v)) (Map.toList commonMetadata) 
                    hPutStrLn h ".. properties .."
                    mapM_ (\(k,v) -> do
                            let minCount = Map.findWithDefault "" "minCount" v
                            let maxCount = Map.findWithDefault "" "maxCount" v
                            let typeText = case Map.lookup "type" v of 
                                        Just t -> " : " <> t <> " [" <> minCount <> ".." <> maxCount <> "]"
                                        _ -> ""
                            T.hPutStrLn h ("    " <> k <> typeText)
                        ) (Map.toList custom) 
                    hPutStrLn h "}"
                    T.hPutStrLn h ("note top of " <> commonName)
                    T.hPutStrLn h (T.unlines ["<b>Summary</b>", commonSummary, "<b>Description</b>", commonDescription])
                    T.hPutStrLn h "end note"
                    case Map.lookup "SubclassOf" commonMetadata of
                        Just sco ->
                            unless (sco == "none" || isBasicType sco) $
                                T.hPutStrLn h ("\"" <> nameToIdentifier sco <> "\" <|-[thickness=4]- \"" <> commonName <> "\"")

                        Nothing -> pure ()

                    mapM_ (\(k,v) -> do
                            case Map.lookup "type" v of 
                                Just t -> if isBasicType t
                                          then pure ()
                                          else T.hPutStrLn h ("\"" <> nameToIdentifier t <> "\" <--- \"" <> commonName <> "::" <> k <> "\"")
                                _ -> pure ()
                            -- T.hPutStrLn h ("note right of " <> commonName <> "::" <> k)
                            -- T.hPutStrLn h ("    " <> T.pack (show v))
                            -- T.hPutStrLn h "end note"
                        ) (Map.toList custom) 
            hPutStrLn h "@enduml"
        callProcess "plantuml" ["-tsvg", out] 


        withFile outCommented WriteMode $ \h -> do
            hPutStrLn h "@startuml"
            case Map.lookup key classes of
                Nothing -> hPutStrLn h "note: failed to find class"
                Just (SpecCommon
                        { commonName = commonName
                        , commonStart = commonStart
                        , commonSummary = commonSummary
                        , commonDescription = commonDescription
                        , commonMetadata = commonMetadata
                        , custom = custom
                }) -> do
                    hPutStrLn h ("!include_once ../" ++ relativePath)
            hPutStrLn h "@enduml"
        -- callProcess "plantuml" ["-tsvg", outCommented] 

        return relativePath

classesToPuml :: FilePath -> Spec -> IO FilePath
classesToPuml root context@(Spec{classes = classes}) = let
        relativePath = "Classes" </> "index" <.> "puml"
        out = root </> relativePath
        outCommented = out -<.> "commented.puml"
    in do
        cPumls <- mapM (classToPuml root context) (Map.keys classes)

        withFile out WriteMode $ \h -> do
            hPutStrLn h "@startuml"
            mapM_ (\puml -> hPutStrLn h ("!include_once ../" ++ puml)) cPumls
            hPutStrLn h "@enduml"
        callProcess "plantuml" ["-tsvg", out] 

        withFile outCommented WriteMode $ \h -> do
            hPutStrLn h "@startuml"
            mapM_ (\puml -> hPutStrLn h ("!include_once ../" ++ (puml -<.> "commented.puml"))) cPumls
            hPutStrLn h "@enduml"
        -- callProcess "plantuml" ["-tsvg", outCommented] 

        return relativePath


specToPuml :: FilePath -> Spec -> IO FilePath
specToPuml root context = let
        relativePath = "index" <.> "puml"
        out = root </> relativePath
        outCommented = out -<.> "commented.puml"
    in do
        vPuml <- vocabulariesToPuml root context
        cPuml <- classesToPuml root context

        withFile out WriteMode $ \h -> do
            hPutStrLn h "@startuml"
            hPutStrLn h ("!include_once " ++ vPuml)
            hPutStrLn h ("!include_once " ++ cPuml)
            hPutStrLn h "@enduml"
        callProcess "plantuml" ["-tsvg", out] 

        withFile outCommented WriteMode $ \h -> do
            hPutStrLn h "@startuml"
            hPutStrLn h ("!include_once " ++ (vPuml -<.> "commented.puml"))
            hPutStrLn h ("!include_once " ++ (cPuml -<.> "commented.puml"))
            hPutStrLn h "@enduml"
        -- callProcess "plantuml" ["-tsvg", outCommented] 

        return relativePath
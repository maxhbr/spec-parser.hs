module MyPrelude
    ( module X
    ) where

import           Control.Monad as X
import           Data.Aeson as X
import           Data.Aeson.Encode.Pretty as X
import           Data.Either as X (fromRight)
import           Data.List as X (isInfixOf, isSuffixOf, nub)
import           Data.Maybe as X (fromMaybe, fromJust, isJust)
import           Data.Text as X (Text)
import           Debug.Trace as X
import           System.Directory as X
import           System.FilePath as X
import           System.FilePath.Glob as X (glob)
import           System.IO as X
import           System.Log.Formatter as X
import           System.Log.Handler as X (setFormatter)
import           System.Log.Handler.Simple as X
import           System.Log.Handler.Syslog as X
import           System.Log.Logger as X
import           System.Process as X
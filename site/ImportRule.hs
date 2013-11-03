{-# OPTIONS_GHC -w #-}
module ImportRule () where

import qualified Control.Monad                          as Monad
import qualified Control.Monad.Logger                   as MonadLogger
import qualified Control.Applicative                    as Applicative
import qualified Control.Exception                      as Exc
import qualified Control.Concurrent                     as Concurrent
import qualified Control.Concurrent.Chan                as Chan

import qualified Data.Aeson                             as Aeson
import qualified Data.Aeson.Types                       as AesonTypes
import qualified Data.Bits                              as Bits
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Char8                  as BSC
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.Char                              as Char
import qualified Data.Conduit                           as Conduit
import qualified Data.Conduit.Binary                    as ConduitB
import qualified Data.Conduit.List                      as ConduitL
import qualified Data.Default                           as Default
import qualified Data.IORef                             as IORef
import qualified Data.List                              as List
import qualified Data.Maybe                             as Maybe
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as TE
import qualified Data.Word                              as Word
import qualified Data.Yaml                              as Yaml

import qualified Language.Haskell.TH                    as TH
import qualified Language.Haskell.TH.Syntax             as THS

import qualified Network.HTTP.Conduit                   as HConduit
import qualified Network.Wai.Middleware.RequestLogger   as RequestLogger
import qualified Network.Wai.Handler.Warp               as Warp

import qualified System.Directory                       as Directory
import qualified System.Exit                            as Exit
import qualified System.IO                              as I
import qualified System.Log.FastLogger                  as FastLogger
import qualified System.Mem                             as Mem
import qualified System.Environment                     as Environment

import qualified Text.Hamlet                            as Hamlet
import qualified Text.Jasmine                           as Jasmine
import qualified Text.Shakespeare.Text                  as Text

import qualified Yesod                                  as Y
import qualified Yesod.Auth                             as YAuth
import qualified Yesod.Auth.HashDB                      as YAuthHDB
import qualified Yesod.Default.Config                   as YDConfig
import qualified Yesod.Default.Handlers                 as YDHandlers
import qualified Yesod.Default.Main                     as YDMain
import qualified Yesod.Default.Util                     as YDUtil
import qualified Yesod.Form                             as YForm
import qualified Yesod.Static                           as YStatic

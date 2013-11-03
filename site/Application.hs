{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import qualified Yesod                                  as Y
import qualified Yesod.Default.Config                   as YDConfig
import qualified Yesod.Default.Main                     as YDMain
import qualified Data.Default                           as Default
import qualified Network.Wai.Middleware.RequestLogger   as RequestLogger
import qualified Network.HTTP.Conduit                   as HConduit
import qualified System.IO                              as I
import qualified System.Log.FastLogger                  as FastLogger

import Yesod.Default.Handlers (getFaviconR, getRobotsR)

import Foundation (resourcesApp, App(App, appLogger), getStatic, Route(..))
import Settings (Extra(..), parseExtra)
import Settings.StaticFiles (staticSite)
import Settings.Development (development)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home (getHomeR, postHomeR)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
Y.mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: YDConfig.AppConfig YDConfig.DefaultEnv Extra -> IO Y.Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- RequestLogger.mkRequestLogger Default.def
        { RequestLogger.outputFormat =
            if development
                then RequestLogger.Detailed True
                else RequestLogger.Apache RequestLogger.FromSocket
        , RequestLogger.destination = RequestLogger.Logger $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- Y.toWaiAppPlain foundation
    return $ logWare app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: YDConfig.AppConfig YDConfig.DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- HConduit.newManager Default.def
    s <- staticSite
    logger <- FastLogger.mkLogger True I.stdout
    let foundation = App conf s manager logger

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Y.Application)
getApplicationDev =
    YDMain.defaultDevelApp loader makeApplication
  where
    loader = YDConfig.loadConfig (YDConfig.configSettings YDConfig.Development)
        { YDConfig.csParseExtra = parseExtra
        }

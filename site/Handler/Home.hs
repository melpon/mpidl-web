{-# LANGUAGE TupleSections, OverloadedStrings, ViewPatterns #-}
module Handler.Home
  ( getHomeR
  , postHomeR
  ) where

import Import
import qualified Yesod                                  as Y
import qualified Data.Maybe                             as Maybe
import qualified Data.Text                              as T
import qualified Data.Text.Lazy                         as TL
import qualified Filesystem                             as FS
import qualified Shelly                                 as Shelly

import Text.Shakespeare.Text (st, lt)
import Shelly ((</>))

import Foundation (Handler, Route(HomeR))
import Settings (widgetFile)

defaultCode :: T.Text
defaultCode = [st|
message hoge {
  0: int moge
  1: map<string, double> hage
}

service test {
  void foo(0: hoge x)
}
|]

getHomeR :: Handler Y.Html
getHomeR = do
    let submission = Nothing :: Maybe (Y.FileInfo, T.Text)
        handlerName = "getHomeR" :: T.Text
    Y.defaultLayout $ do
        aDomId <- Y.newIdent
        Y.setTitle "MessagePack IDL Code Generator"
        $(widgetFile "homepage")

postHomeR :: Handler (Y.ContentType, Y.Content)
postHomeR = do
  (Maybe.fromMaybe "noname" -> name, source, lang, namespace) <- Y.runInputPost $ (,,,)
    <$> Y.iopt Y.textField "name"
    <*> Y.ireq Y.textField "source"
    <*> Y.ireq Y.textField "lang"
    <*> Y.iopt Y.textField "namespace"

  let tarname = [lt|#{name}.tar.bz2|]
      idlname = [lt|#{name}.idl|]

  let opts = map TL.fromStrict $ case (lang, namespace) of
        ("cpp",  Just ns) -> ["-n", ns]
        ("java", Just pn) -> ["-p", pn]
        ("ruby", Just mn) -> ["-m", mn]
        _ -> []
  
  archive <- Shelly.shelly $ do
    Shelly.withTmpDir $ \tmppath -> do
      let tmpIdlname = tmppath </> Shelly.fromText idlname
      let tmpName = tmppath </> name
      let tmpTarname = tmppath </> Shelly.fromText tarname
      Shelly.writefile tmpIdlname $ TL.fromStrict source
      Shelly.run_ "msgpack-idl/bin/mpidl" $ [TL.fromStrict lang, "-o", Shelly.toTextIgnore tmpName, Shelly.toTextIgnore tmpIdlname] ++ opts
      Shelly.chdir tmppath $ do
        Shelly.run_ "tar" ["-cjf", tarname, TL.fromStrict name]
      Y.liftIO $ FS.readFile $ tmpTarname

  Y.addHeader "Content-Disposition" [st|attachment; filename="#{tarname}"|]
  return ("application/x-bz2", Y.toContent archive)

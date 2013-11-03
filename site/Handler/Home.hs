{-# LANGUAGE TupleSections, OverloadedStrings, ViewPatterns #-}
module Handler.Home
  ( getHomeR
  , postHomeR
  ) where

import Import
import qualified Yesod                                  as Y
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
    Y.defaultLayout $ do
        Y.setTitle "MessagePack IDL Code Generator"
        $(widgetFile "homepage")

sanitize :: T.Text -> T.Text
sanitize name =
  let name' = T.filter (\c -> isAlpha c || isDigit c || any (c==) ['_', '-', '.']) name
   in if T.length name' == 0
        then "noname"
        else name'
  where
    isAlpha c = 'a' <= c && c <= 'z' ||
                'A' <= c && c <= 'Z'
    isDigit c = '0' <= c && c <= '9'

runMpidl :: Y.MonadIO m => T.Text -> T.Text -> T.Text -> Maybe T.Text -> (Shelly.FilePath -> Shelly.Sh a) -> m a
runMpidl name source lang namespace resultFunc = do
  let idlname = [lt|#{name}.idl|]

  let opts = map TL.fromStrict $ case (lang, namespace) of
        ("cpp",  Just ns) -> ["-n", ns]
        ("java", Just pn) -> ["-p", pn]
        ("ruby", Just mn) -> ["-m", mn]
        _ -> []
  
  Shelly.shelly $ do
    Shelly.withTmpDir $ \tmppath -> do
      let tmpName = tmppath </> name
      let tmpIdlname = tmppath </> Shelly.fromText idlname
      Shelly.writefile tmpIdlname $ TL.fromStrict source
      Shelly.run_ "msgpack-idl/bin/mpidl" $ [TL.fromStrict lang, "-o", Shelly.toTextIgnore tmpName, Shelly.toTextIgnore tmpIdlname] ++ opts
      resultFunc tmppath

postHomeR :: Handler (Y.ContentType, Y.Content)
postHomeR = do
    (maybe "noname" sanitize -> name, source, lang, namespace) <- Y.runInputPost $ (,,,)
      <$> Y.iopt Y.textField "name"
      <*> Y.ireq Y.textField "source"
      <*> Y.ireq Y.textField "lang"
      <*> Y.iopt Y.textField "namespace"

    let tarname = [lt|#{name}.tar.bz2|]
    archive <- runMpidl name source lang namespace (returnTar name tarname)

    Y.addHeader "Content-Disposition" [st|attachment; filename="#{tarname}"|]
    return ("application/x-bz2", Y.toContent archive)

  where
    returnTar name tarname tmppath = do
      let tmpTarname = tmppath </> Shelly.fromText tarname
      Shelly.chdir tmppath $ do
        Shelly.run_ "tar" ["-cjf", tarname, TL.fromStrict name]
      Y.liftIO $ FS.readFile $ tmpTarname

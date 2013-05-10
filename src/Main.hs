{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
module Main where

import qualified Network.MPD as MPD
import Happstack.Server
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Char8 as BS
import Data.Text
import Data.Text.Encoding
import Data.Aeson 
import Data.Aeson.TH
import Data.Map.Lazy

mapping :: ServerPartT IO Response
mapping = msum [
        dir "play" play,
        dir "stop" stop,
        dir "list" playlist,
        dir "files" $ path filelist,
        dir "files" $ filelist ""
        ]

main :: IO ()
main = simpleHTTP nullConf mapping

play :: ServerPartT IO Response
play = do
     res <- liftIO $ MPD.withMPD $ MPD.play Nothing
     simpleReply res

stop :: ServerPartT IO Response
stop = do
     res <- liftIO $ MPD.withMPD $ MPD.stop
     simpleReply res

playlist = do
         res <- liftIO $ MPD.withMPD $ MPD.playlistInfo Nothing
         case res of
              Left err -> internalServerError $ toResponse $ toJSON (object ["Error" .= err])
              Right list -> ok $ toResponse $ toJSON (Prelude.take 5 list)

filelist p = do
         res <- liftIO $ MPD.withMPD $ MPD.lsInfo p
         case res of
              Left err -> internalServerError $ toResponse $ toJSON (object ["Error" .= err])
              Right list -> ok $ toResponse $ toJSON (Prelude.take 5 list)


simpleReply a = case a of
          Left err -> internalServerError $ toResponse $ toJSON (object ["Error" .= err])
          Right yay -> ok $ toResponse $ toJSON yay

instance ToMessage Value where
                   toMessage s = encode s
                   toContentType _ = BS.pack "application/json" 

instance FromReqURI MPD.Path where
         fromReqURI a = Just $ MPD.Path (BS.pack a)

$(deriveJSON id ''MPD.Metadata)

instance ToJSON (Map MPD.Metadata [MPD.Value]) where
         toJSON m = object $ elems keyval
                where 
                      keyval = mapWithKey (\a b -> (Data.Text.pack.show $ a) .= b) m

instance FromJSON (Map MPD.Metadata [MPD.Value]) where
         parseJSON _ = mzero

$(deriveJSON id ''MPD.MPDError)
$(deriveJSON id ''MPD.ACKType)
$(deriveJSON id ''MPD.Value)
$(deriveJSON id ''MPD.Id)
$(deriveJSON id ''MPD.Path)
-- $(deriveJSON (Prelude.drop 2) ''MPD.Song)
$(deriveJSON id ''MPD.Song)
$(deriveJSON id ''MPD.LsResult)
$(deriveJSON id ''MPD.PlaylistName)
-- $(deriveJSON (Prelude.drop 2) ''MPD.LsResult)
-- $(deriveJSON (Prelude.drop 2) ''MPD.PlaylistName)

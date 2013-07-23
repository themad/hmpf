{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as MPDx
import Happstack.Server
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Char8 as BS
import Data.Text
-- import Data.Text.Encoding ()
import Data.Aeson 
import Data.Aeson.TH
import Data.Map.Lazy
import Control.Applicative (optional)
import Data.Maybe (fromMaybe)

class SimpleReply a where
      simpleReply :: a -> ServerPartT IO Response

mapping :: ServerPartT IO Response
mapping = msum [
        dir "play" $ path (\s->play $ Just s),
        dir "play" $ play Nothing,
        dir "stop" stop,
        dir "toggle" toggle,
        dir "resume" resume,
        dir "pause" stop,
        dir "next" next,
        dir "previous" previous,
        dir "list" playlist,
        dir "files" $ path filelist,
        dir "files" $ filelist "",
        dir "playlists" $ playlists,
        dir "status" $ status,
        dir "/" $  status
        ]

main :: IO ()
main = simpleHTTP nullConf mapping

play :: Maybe Int -> ServerPartT IO Response
play a = do
     res <- liftIO $ MPD.withMPD $ MPD.play a
     simpleReply res

next :: ServerPartT IO Response
next = do
     res <- liftIO $ MPD.withMPD $ MPD.next
     simpleReply res

previous :: ServerPartT IO Response
previous = do
     res <- liftIO $ MPD.withMPD $ MPD.previous
     simpleReply res


stop :: ServerPartT IO Response
stop = do
     res <- liftIO $ MPD.withMPD $ MPD.stop
     simpleReply res

status :: ServerPartT IO Response
status = do
     res <- liftIO $ MPD.withMPD $ MPD.status
     simpleReply res

pause :: ServerPartT IO Response
pause = do
     res <- liftIO $ MPD.withMPD $ MPD.pause True
     simpleReply res

resume :: ServerPartT IO Response
resume = do
     res <- liftIO $ MPD.withMPD $ MPD.pause False
     simpleReply res

toggle :: ServerPartT IO Response
toggle = do
     res <- liftIO $ MPD.withMPD $ MPDx.toggle
     simpleReply res

playlist :: ServerPartT IO Response
playlist = msum [ playlistIndex, path (\s->playlistAdd s) ]

playlistAdd s = do
         method POST
         res <- liftIO $ MPD.withMPD $ MPD.add_ s
         case res of
           Left err -> case err of
                MPD.ACK a s -> case a of
                    MPD.FileNotFound -> notFound $ toResponse $ toJSON (object ["Error" .= err])
                    _ -> simpleReply res
                _ -> simpleReply res
           Right yay -> simpleReply res

playlistIndex = do
         method GET
         res <- liftIO $ MPD.withMPD $ MPD.playlistInfo Nothing
         simpleReply res

filelist p = do
         res <- liftIO $ MPD.withMPD $ MPD.lsInfo p
         case res of
           Left err -> case err of
                MPD.ACK a s -> case a of
                    MPD.FileNotFound -> notFound $ toResponse $ toJSON (object ["Error" .= err])
                    _ -> simpleReply res
                _ -> simpleReply res
           Right yay -> simpleReply res


playlists = msum [ playlistsIndex, path (\s->playlistsLoad s) ]

playlistsLoad s = do
         method POST
         res <- liftIO $ MPD.withMPD $ MPD.load s
         simpleReply res

playlistsIndex = do
         method GET
         res <- liftIO $ MPD.withMPD $ MPD.listPlaylists
         simpleReply res

instance SimpleReply (MPD.Response MPD.Status) where
         simpleReply a = case a of
           Left err -> internalServerError $ toResponse $ toJSON (object ["Error" .= err])
           Right yay -> ok $ toResponse $ toJSON yay
           
instance (ToJSON x) => SimpleReply (MPD.Response [x]) where
         simpleReply a = case a of
           Left err -> internalServerError $ toResponse $ toJSON (object ["Error" .= err])
           Right yay -> paginate yay >>= ok . toResponse . toJSON

instance SimpleReply (MPD.Response ()) where
         simpleReply a = case a of
           Left err -> internalServerError $ toResponse $ toJSON (object ["Error" .= err])
           Right yay -> ok $ toResponse $ toJSON yay

paginate :: [a] -> ServerPartT IO [a]
paginate l = do
        startnum_ <- optional $ lookRead "start"
        let s = (fromMaybe 1 startnum_) - 1
        msum [withEnd s, withoutEnd s]
        where 
          withEnd s = do
              endnum <- lookRead "count"
              return $ Prelude.take endnum $ Prelude.drop s l
          withoutEnd s = return $ Prelude.drop s l

instance ToMessage Value where
                   toMessage s = encode s
                   toContentType _ = BS.pack "application/json" 

instance FromReqURI MPD.PlaylistName where
         fromReqURI a = Just $ MPD.PlaylistName (BS.pack a)


instance FromReqURI MPD.Path where
         fromReqURI a = Just $ MPD.Path (BS.pack a)

$(deriveJSON (Prelude.drop 2) ''MPD.Metadata)

instance ToJSON (Map MPD.Metadata [MPD.Value]) where
         toJSON m = object $ elems keyval
                where 
                      keyval = mapWithKey (\a b -> (Data.Text.pack.show $ a) .= b) m

instance FromJSON (Map MPD.Metadata [MPD.Value]) where
         parseJSON _ = mzero


-- $(deriveJSON (Prelude.drop 2) ''MPD.LsResult)
-- warum funktioniert das da nicht???
instance ToJSON MPD.LsResult where
         toJSON (MPD.LsDirectory a) = object $ ["Directory" .= a]
         toJSON (MPD.LsSong a) = object $ ["Song" .= a]
         toJSON (MPD.LsPlaylist a) = object $ ["Playlist" .= a]

instance FromJSON MPD.LsResult where
         parseJSON _ = mzero

$(deriveJSON id ''MPD.MPDError)
$(deriveJSON (Prelude.drop 2) ''MPD.Status)
$(deriveJSON (Prelude.drop 2) ''MPD.State)
$(deriveJSON id ''MPD.ACKType)
$(deriveJSON (Prelude.drop 2) ''MPD.Value)
$(deriveJSON id ''MPD.Id)
$(deriveJSON id ''MPD.Path)
-- $(deriveJSON (Prelude.drop 2) ''MPD.Song)
$(deriveJSON (Prelude.drop 2) ''MPD.Song)

$(deriveJSON id ''MPD.PlaylistName)
-- $(deriveJSON (Prelude.drop 2) ''MPD.LsResult)
-- $(deriveJSON (Prelude.drop 2) ''MPD.PlaylistName)

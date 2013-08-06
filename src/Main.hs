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
import Data.Map.Lazy (Map, mapWithKey, elems)
import Control.Applicative (optional)
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (insert)
import qualified Data.List as L

class SimpleReply a where
      simpleReply :: a -> ServerPartT IO Response

data Paginated a = Paginated {
     start :: Int,
     count :: Int,
     total :: Int,
     result:: [a] }

$(deriveJSON id ''Paginated)

mapping :: ServerPartT IO Response
mapping = msum [
        dir "play" $ path (\s->play $ Just s),
        dir "play" $ play Nothing,
        dir "delete" $ path delete,
        dir "stop" stop,
        dir "toggle" toggle,
        dir "shuffle" shuffle,
        dir "clear" clear,
        dir "resume" resume,
        dir "pause" pause,
        dir "next" next,
        dir "previous" previous,
        dir "prev" previous,
        dir "list" playlist,
        dir "files" $ restPath filelist,
        dir "files" $ filelist "",
        dir "playlists" $ playlists,
        dir "status" $ status,
        serveDirectory EnableBrowsing ["index.html"] "static",
        notFound $ toResponse $ ("hmpf: HTTP file not found (error 404)."::ByteString)
        ]

main :: IO ()
main = Prelude.putStrLn "Starting server..." >>  simpleHTTP nullConf mapping

restPath :: (MonadPlus m, FromReqURI a, ServerMonad m) => (a -> m b) -> m b
restPath handle = do
     rq <- askRq
     case rqPaths rq of
          [] -> mzero
          x  -> maybe mzero (\s -> localRq (\newRq -> newRq{rqPaths = []}) (handle s)) (fromReqURI (L.intercalate "/" x))

play :: Maybe Int -> ServerPartT IO Response
play a = do
     method POST
     nullDir
     res <- liftIO $ MPD.withMPD $ MPD.play a
     simpleReply res

shuffle :: ServerPartT IO Response
shuffle = do
     method POST
     nullDir
     res <- liftIO $ MPD.withMPD $ MPD.shuffle Nothing
     simpleReply res

delete :: Int -> ServerPartT IO Response
delete a = do
     method POST
     nullDir
     res <- liftIO $ MPD.withMPD $ MPD.delete a
     simpleReply res

clear :: ServerPartT IO Response
clear = do
     method POST
     nullDir
     res <- liftIO $ MPD.withMPD $ MPD.clear
     simpleReply res

next :: ServerPartT IO Response
next = do
     method POST
     nullDir
     res <- liftIO $ MPD.withMPD $ MPD.next
     simpleReply res

previous :: ServerPartT IO Response
previous = do
     method POST
     nullDir
     res <- liftIO $ MPD.withMPD $ MPD.previous
     simpleReply res


stop :: ServerPartT IO Response
stop = do
     method POST
     nullDir
     res <- liftIO $ MPD.withMPD $ MPD.stop
     simpleReply res

status :: ServerPartT IO Response
status = do
     decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
     _ <- optional $ msum [setCrossfade, setRandom, setRepeat, setConsume, setSingle]
     res <- liftIO $ MPD.withMPD $ MPD.status
     case res of
           Right x -> do
             songinfo <- liftIO $ MPD.withMPD $ MPD.currentSong
             case songinfo of
                  (Right (Just s)) -> ok $ toResponse $ combine x (toJSON s)
                  _ -> ok $ toResponse $ combine x Null
           _ -> simpleReply res
     where 
           combine a b = case toJSON a of 
                   Object o -> Object $ insert "Song" b o
                   _ -> toJSON a

setCrossfade :: ServerPartT IO Response
setCrossfade = do
     method POST
     value <- lookRead "Crossfade"
     _ <- liftIO $ MPD.withMPD $ MPD.crossfade value
     mzero

setRepeat :: ServerPartT IO Response
setRepeat = do
     method POST
     value <- lookRead "Repeat"
     _ <- liftIO $ MPD.withMPD $ MPD.repeat value
     mzero

setSingle :: ServerPartT IO Response
setSingle = do
     method POST
     value <- lookRead "Single"
     _ <- liftIO $ MPD.withMPD $ MPD.single value
     mzero

setRandom :: ServerPartT IO Response
setRandom = do
     method POST
     value <- lookRead "Random"
     _ <- liftIO $ MPD.withMPD $ MPD.random value
     mzero

setConsume :: ServerPartT IO Response
setConsume = do
     method POST
     value <- lookRead "Consume"
     _ <- liftIO $ MPD.withMPD $ MPD.consume value
     mzero

pause :: ServerPartT IO Response
pause = do
     method POST
     nullDir
     res <- liftIO $ MPD.withMPD $ MPD.pause True
     simpleReply res

resume :: ServerPartT IO Response
resume = do
     method POST
     nullDir
     res <- liftIO $ MPD.withMPD $ MPD.pause False
     simpleReply res

toggle :: ServerPartT IO Response
toggle = do
     method POST
     nullDir
     res <- liftIO $ MPD.withMPD $ MPDx.toggle
     simpleReply res

playlist :: ServerPartT IO Response
playlist = msum [ playlistIndex, restPath playlistAdd ]

playlistAdd :: MPD.Path -> ServerPartT IO Response
playlistAdd s = do
         method POST
         nullDir
         res <- liftIO $ MPD.withMPD $ MPD.add_ s
         case res of
           Left err -> case err of
                MPD.ACK a _ -> case a of
                    MPD.FileNotFound -> notFound $ toResponse $ toJSON (object ["Error" .= err])
                    _ -> simpleReply res
                _ -> simpleReply res
           _ -> simpleReply res

playlistIndex :: ServerPartT IO Response
playlistIndex = do
         method GET
         nullDir
         res <- liftIO $ MPD.withMPD $ MPD.playlistInfo Nothing
         simpleReply res

filelist :: MPD.Path -> ServerPartT IO Response
filelist p = do
         method GET
         nullDir
         res <- liftIO $ MPD.withMPD $ MPD.lsInfo p
         case res of
           Left err -> case err of
                MPD.ACK a _ -> case a of
                    MPD.FileNotFound -> notFound $ toResponse $ toJSON (object ["Error" .= err])
                    _ -> simpleReply res
                _ -> simpleReply res
           _ -> simpleReply res


playlists :: ServerPartT IO Response
playlists = msum [ playlistsIndex, restPath playlistsLoad ]

playlistsLoad :: MPD.PlaylistName -> ServerPartT IO Response
playlistsLoad s = do
         method POST
         nullDir
         res <- liftIO $ MPD.withMPD $ MPD.load s
         simpleReply res

playlistsIndex :: ServerPartT IO Response
playlistsIndex = do
         method GET
         nullDir
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

paginate :: [a] -> ServerPartT IO (Paginated a)
paginate l = do
        startnum_ <- optional $ lookRead "start"
        let s = (fromMaybe 0 startnum_)
        msum [withEnd s, withoutEnd s]
        where 
          withEnd s = do
              endnum <- lookRead "count"
              let res = Prelude.take endnum $ Prelude.drop s l
              return $ Paginated { Main.result = res, start=s, Main.count=(Prelude.length res), total=(Prelude.length l) }
          withoutEnd s = return $ Paginated { Main.result = ( Prelude.drop s l), start=s, Main.count=(Prelude.length l), total=(Prelude.length l) }

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

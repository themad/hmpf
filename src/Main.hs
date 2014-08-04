{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as MPDx
import Happstack.Server
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Char8 as BS
import Data.Text
import Data.Text.Encoding
import Data.Aeson 
import Data.Aeson.TH
import Data.Map.Lazy (Map, mapWithKey, elems)
import Data.Monoid
import Control.Applicative (optional, pure)
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (insert)
import qualified Data.List as L
import Data.Typeable
import System.Console.CmdArgs

class SimpleReply a where
      simpleReply :: a -> ServerPartT IO Response

data Paginated a = Paginated {
     start :: Int,
     count :: Int,
     total :: Int,
     result:: [a] }

data Configuration = Configuration {
     confPort :: Int,
     confStaticDir :: FilePath     
     } deriving (Show, Data, Typeable)

$(deriveJSON Data.Aeson.TH.defaultOptions ''Paginated)

debug :: ServerPartT IO b
debug = do
        r <- askRq
        liftIO $ Prelude.putStrLn (show $ rqPaths r)
        mzero

mapping :: Configuration -> ServerPartT IO Response
mapping conf = msum [
--        debug,
        dir "play" $ path (\s->play $ Just s),
        dir "play" $ play Nothing,
        dir "delete" $ path delete,
        dir "stop" stop,
        dir "toggle" toggle,
        dir "shuffle" shuffle,
        dir "seek" $ path seek,
        dir "clear" clear,
        dir "resume" resume,
        dir "pause" pause,
        dir "search" search,
        dir "next" next,
        dir "previous" previous,
        dir "prev" previous,
        dir "list" playlist,
        dir "files" $ restPath filelist,
        dir "files" $ filelist "",
        dir "playlists" $ playlists,
        dir "status" $ status,
        serveDirectory EnableBrowsing ["index.html"] (confStaticDir conf),
        notFound $ toResponse $ ("hmpf: HTTP file not found (error 404)."::ByteString)
        ]

config :: Configuration
config = Configuration { 
       confPort = (8000::Int) &= help "Port to listen on" &= name "port" &= explicit,
       confStaticDir = ("static" :: FilePath) &= help "Static file directory" &= name "static" &= explicit &= typ "dir"
       } &= summary "hmpf v0.01 (C) 2014 themad, 0mark" &= program "hmpf" 

main :: IO ()
main = do
     conf <- cmdArgs config
     Prelude.putStr "Starting server on port " 
     Prelude.putStrLn $ show $ confPort conf
     simpleHTTP nullConf{port = confPort conf} $ mapping conf

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

seek :: Integer -> ServerPartT IO Response
seek a = do
     method POST
     nullDir
     state <- liftIO $ MPD.withMPD $ MPD.status
     case state of
          Right MPD.Status{MPD.stSongPos=r} -> do
                case r of 
                     Just song -> simpleReply =<< (liftIO $ MPD.withMPD $ MPD.seek song a)
                     _ -> badRequest $ toResponse $ toJSON (object ["Error" .= String "Not currently playing a song"]) 
          _ -> simpleReply state

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
     _ <- optional $ msum [setCrossfade, setRandom, setRepeat, setConsume, setSingle, setSeek]
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

search :: ServerPartT IO Response
search = do
     method GET
     value <- lookRead "q"
     reses <- mapM (\s -> liftIO $ MPD.withMPD $ MPD.search $ s MPD.=? value) [MPD.Artist, MPD.Title, MPD.Album, MPD.Name]
     let res = mconcat $ L.map etm reses 
     simpleReply $ (Right res :: MPD.Response [MPD.Song])
     where 
          etm = either mempty id

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

setSeek :: ServerPartT IO Response
setSeek = do
     method POST
     value <- lookRead "Time"
     state <- liftIO $ MPD.withMPD $ MPD.status
     case state of
          Right MPD.Status{MPD.stSongPos=r} -> do
                case r of 
                     Just song -> simpleReply =<< (liftIO $ MPD.withMPD $ MPD.seek song value)
                     _ -> mzero
          _ -> mzero

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
           Left err -> badRequest $ toResponse $ toJSON (object ["Error" .= err])
           Right yay -> ok $ toResponse $ toJSON yay
           
instance (ToJSON x) => SimpleReply (MPD.Response [x]) where
         simpleReply a = case a of
           Left err -> badRequest $ toResponse $ toJSON (object ["Error" .= err])
           Right yay -> paginate yay >>= ok . toResponse . toJSON

instance SimpleReply (MPD.Response ()) where
         simpleReply a = case a of
           Left err -> badRequest $ toResponse $ toJSON (object ["Error" .= err])
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
                   toContentType _ = BS.pack "application/json; charset=UTF-8" 

instance FromReqURI MPD.PlaylistName where
         fromReqURI a = Just $ MPD.PlaylistName $ encodeUtf8 $ Data.Text.pack a

instance FromReqURI MPD.Path where
         fromReqURI a = Just $ MPD.Path $ encodeUtf8 $ Data.Text.pack a

instance FromReqURI MPD.Value where
         fromReqURI a = Just $ MPD.Value $ encodeUtf8 $ Data.Text.pack a

$(deriveJSON Data.Aeson.TH.defaultOptions{fieldLabelModifier=(Prelude.drop 2)} ''MPD.Metadata)

instance ToJSON (Map MPD.Metadata [MPD.Value]) where
         toJSON m = object $ elems keyval
                where 
                      keyval = mapWithKey (\a b -> (Data.Text.pack.show $ a) .= b) m

instance FromJSON (Map MPD.Metadata [MPD.Value]) where
         parseJSON _ = mzero

-- $(deriveJSON Data.Aeson.TH.defaultOptions{fieldLabelModifier=(Prelude.drop 2)} ''MPD.LsResult)
-- warum funktioniert das da nicht???
instance ToJSON MPD.LsResult where
         toJSON (MPD.LsDirectory a) = object $ ["Directory" .= a]
         toJSON (MPD.LsSong a) = object $ ["Song" .= a]
         toJSON (MPD.LsPlaylist a) = object $ ["Playlist" .= a]

instance FromJSON MPD.LsResult where
         parseJSON _ = mzero

instance FromJSON ByteString where
    parseJSON (String a) = pure . encodeUtf8 $ a
    parseJSON _ = mzero

instance ToJSON ByteString where
    toJSON a = String . decodeUtf8 $ a

$(deriveJSON Data.Aeson.TH.defaultOptions ''MPD.Value)
$(deriveJSON Data.Aeson.TH.defaultOptions ''MPD.Id)
$(deriveJSON Data.Aeson.TH.defaultOptions ''MPD.MPDError)
$(deriveJSON Data.Aeson.TH.defaultOptions{fieldLabelModifier=(Prelude.drop 2)} ''MPD.Status)
$(deriveJSON Data.Aeson.TH.defaultOptions{fieldLabelModifier=(Prelude.drop 2)} ''MPD.State)
$(deriveJSON Data.Aeson.TH.defaultOptions ''MPD.ACKType)

$(deriveJSON Data.Aeson.TH.defaultOptions ''MPD.Path)
-- $(deriveJSON Data.Aeson.TH.defaultOptions{fieldLabelModifier=(Prelude.drop 2)} ''MPD.Song)
$(deriveJSON Data.Aeson.TH.defaultOptions{fieldLabelModifier=(Prelude.drop 2)} ''MPD.Song)

$(deriveJSON Data.Aeson.TH.defaultOptions ''MPD.PlaylistName)
-- $(deriveJSON Data.Aeson.TH.defaultOptions{fieldLabelModifier=(Prelude.drop 2)} ''MPD.LsResult)
-- $(deriveJSON Data.Aeson.TH.defaultOptions{fieldLabelModifier=(Prelude.drop 2)} ''MPD.PlaylistName)

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

newtype ConnectionReader a = ConnectionReader { runCR :: R.Connection -> a }

instance Functor ConnectionReader where
  fmap f cr = ConnectionReader $ \c -> f (runCR cr c)

instance Applicative ConnectionReader where
  pure = return
  ConnectionReader f <*> ConnectionReader a = ConnectionReader $ \c -> (f c) (a c)

instance Monad ConnectionReader where
  return = ConnectionReader . const
  a >>= f = ConnectionReader $ \c -> runCR (f (runCR a c)) c

ask :: ConnectionReader R.Connection
ask = ConnectionReader id

asks :: (R.Connection -> a) -> ConnectionReader a
asks f = fmap f ask

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- Right of arrow is IO Int, so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

shortyGen :: IO String
shortyGen =
  replicateM 7 (randomElement alphaNum)

saveURI :: BC.ByteString
        -> BC.ByteString
        -> ConnectionReader(IO (Either R.Reply R.Status))
saveURI shortURI uri = asks (\conn -> R.runRedis conn $ R.set shortURI uri)

getURI :: BC.ByteString -> ConnectionReader(IO (Either R.Reply (Maybe BC.ByteString)))
getURI shortURI = asks (\conn -> R.runRedis conn $ R.get shortURI)

linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL</a>"
         ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: ", TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a url, did you forget http://?"
            ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

app :: ConnectionReader (ScottyM ())
app = asks (doIt)
  where doIt rConn = do
          get "/" $ do
            uri <- param "uri"
            let parsedUri :: Maybe URI
                parsedUri = parseURI (TL.unpack uri)
            case parsedUri of
              Just _  -> do
                shawty <- liftIO shortyGen
                let shorty = BC.pack shawty
                    uri' = encodeUtf8 (TL.toStrict uri)
                resp <- liftIO $ runCR (saveURI shorty uri') rConn
                html (shortyCreated resp shawty)
              Nothing -> text (shortyAintUri uri)
          get "/:short" $ do
            short <- param "short"
            uri <- liftIO $ runCR (getURI short) rConn
            case uri of
              Left reply -> text (TL.pack (show reply))
              Right mbBS -> case mbBS of
                Nothing -> text "uri not found"
                Just bs -> html (shortyFound tbs)
                  where tbs :: TL.Text
                        tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (runCR app $ rConn)

{-# LANGUAGE OverloadedStrings #-}
module Server where
import Lib
import Control.Monad
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst) -- We are comparing all usernames with firstnames

nicknameExists :: Text -> ServerState -> Bool
nicknameExists nickname = any ((== nickname) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

getClient :: Text -> ServerState -> Client
getClient nickname state = head $ filter ((== nickname) . fst) state 

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, connection) -> WS.sendTextData connection message

sendPrivate :: Text -> Client -> Client -> IO ()
sendPrivate message (nickname1, connection1) (nickname2, connection2) = do
    T.putStrLn $ nickname1 `mappend` " -> " `mappend` nickname2 `mappend` message
    WS.sendTextData connection1 ("to " `mappend` nickname2 `mappend` ": " `mappend` message)
    WS.sendTextData connection2 ("from " `mappend` nickname1 `mappend` ": " `mappend` message) 


main :: IO ()
main = do
    state <- newMVar newServerState -- Why we need to use mvar here? Not just let binding - needs to find out
    WS.runServer "127.0.0.1" 9160 $ application state -- MVar ServerState -> WS.ServerApp

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    clients <- readMVar state
    case msg of
        _   | not (prefix `T.isPrefixOf` msg) 
            -> WS.sendTextData conn ("Wrong announcement" :: Text)
            | any ($ fst client) [T.null, T.any isPunctuation, T.any isSpace] 
            -> WS.sendTextData conn ("Name cannot contain punctuation or whitespace, and cannot be empty" :: Text)
            | clientExists client clients 
            -> WS.sendTextData conn ("User already exists" :: Text)    
            | otherwise 
            -> flip finally disconnect $ do
                modifyMVar_ state $ \s -> do
                    let s' = addClient client s
                    WS.sendTextData conn $
                        "Welcome! Users: " `mappend`
                        T.intercalate ", " (map fst s)
                    broadcast (fst client `mappend` " joined") s'
                    return s'
                talk client state
            where
                prefix     = "Hi! I am " :: Text
                client     = (T.drop (T.length prefix) msg, conn)
                disconnect = do
                    s <- modifyMVar state $ \s ->
                        let s' = removeClient client s in return (s', s')
                    broadcast (fst client `mappend` " disconnected") s

messageHandler :: Client -> Text -> ServerState -> IO ()
messageHandler client message state
    | (privatePrefix `T.isPrefixOf` message) = sendPrivate wmessage client toClient
    | otherwise = broadcast (fst client `mappend` ": " `mappend` message) state
    where
        privatePrefix = "to "
        nicknameMessage = T.drop (T.length privatePrefix) message
        nickname:messageWords = T.words nicknameMessage
        wmessage = T.unwords messageWords
        toClient = getClient nickname state

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= messageHandler (user, conn) msg
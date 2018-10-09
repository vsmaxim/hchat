{-# LANGUAGE OverloadedStrings #-}
module Client where
import Lib
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.IO as T


app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop
    
    loop
    WS.sendClose conn ("Bye" :: Text)


main :: IO ()
main = withSocketsDo $ WS.runClient "localhost" 9160 "/" app
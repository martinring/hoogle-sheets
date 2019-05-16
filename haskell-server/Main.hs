module Main where

import Data.Aeson (encode,decode)
import Data.String
import Data.Maybe (maybe)
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Application.Static as Static
import qualified WaiAppStatic.Types as Static
import qualified Network.Wai.Handler.Warp as Warp
import System.Random (newStdGen)

import Messages

type State = ()

handleSocket :: WS.Connection -> State -> IO ()
handleSocket conn state = do
  let send msg = WS.sendTextData conn (encode msg)
  -- Empfange die Daten vom Client
  msg <- WS.receiveData conn  
  case (decode msg) of 
    Nothing -> do
      putStrLn $ "invalid message from client: '" ++ (show msg) ++ "'"
      handleSocket conn state
    Just (FormulaUpdate x y Nothing) -> do 
      putStrLn $ "clear cell " ++ x:(show y)
      send (CellUpdate x y $ Right (Nothing))
    Just (FormulaUpdate x y (Just formula)) -> do 
      putStrLn $ "update cell " ++ x:(show y) ++ ": " ++ (show formula)
      send (CellUpdate x y $ Left "N/A")
  handleSocket conn state

----------------------------------------------------------------------------

-- Die folgenden funktionen dienen dem generellen Setup der Server Anwendung
-- und sind für die Lösung der Aufgabe nicht relvant.

-- Der statische Teil der Web Anwendung (html + javascript)
static = Static.staticApp (staticSettings)
staticSettings = (Static.defaultFileServerSettings "scala-client/assets")
  -- Caching ausschalten, damit die Anwendung nicht neu gestartet werden
  -- muss, wenn der Scala teil neu Kompiliert wurde.
  { Static.ssMaxAge = Static.NoMaxAge }

-- Der WebSocket
socket pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  handleSocket conn ()

-- Starte die Webanwendung auf Port 3000
main = Warp.run 3000 $
  WS.websocketsOr WS.defaultConnectionOptions socket static
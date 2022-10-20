module Intray.Cli (intrayCli, dispatch) where

import Import
import Intray.Cli.Commands
import Intray.Cli.Env
import Intray.Cli.OptParse
import Network.HTTP.Client.TLS as HTTP
import Servant.Client
import System.FileLock

intrayCli :: IO ()
intrayCli = getInstructions >>= dispatch

dispatch :: Instructions -> IO ()
dispatch (Instructions dispatch settings) = do
  let run = runCliM settings
  case dispatch of
    DispatchRegister rs -> run Shared Shared $ register rs
    DispatchLogin ls -> run Shared Shared $ login ls
    DispatchAddItem t -> run Exclusive Exclusive $ addItem t
    DispatchShowItem -> run Shared Exclusive showItem
    DispatchDoneItem -> run Exclusive Exclusive doneItem
    DispatchSize -> run Shared Exclusive size
    DispatchLogout -> run Shared Shared logout
    DispatchSync -> run Exclusive Exclusive sync
    DispatchReview -> run Exclusive Exclusive review

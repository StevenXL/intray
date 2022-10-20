module Intray.Cli (intrayCli, dispatch) where

import Import
import Intray.Cli.Commands
import Intray.Cli.Env
import Intray.Cli.OptParse
import System.FileLock

intrayCli :: IO ()
intrayCli = getInstructions >>= dispatch

dispatch :: Instructions -> IO ()
dispatch (Instructions d settings) = do
  let run = runCliM settings
  case d of
    DispatchRegister rs -> run Shared Shared $ register rs
    DispatchLogin ls -> run Shared Shared $ login ls
    DispatchAddItem t -> run Exclusive Exclusive $ addItem t
    DispatchShowItem -> run Exclusive Exclusive showItem
    DispatchDoneItem -> run Exclusive Exclusive doneItem
    DispatchSize -> run Shared Exclusive size
    DispatchLogout -> run Shared Shared logout
    DispatchSync -> run Exclusive Exclusive sync
    DispatchReview -> review settings

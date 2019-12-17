{-|

The @export-alka@ command exports all data as JSON compatible
with https://alka.app.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.ExportAlka (
  exportalkamode
 ,exportalka
) where

import Data.Aeson (toJSON)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.IO as T (putStrLn)

import Hledger
import Hledger.Cli.CliOptions

-- | Command line options for this command.
exportalkamode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/ExportAlka.txt")
  []
  [generalflagsgroup2]
  []
  ([], Nothing)

-- | The exportalka command. Exports a full Ledger (Journal plus tree
-- of summed Accounts) as hledger JSON. TODO: convert to alka JSON.
exportalka :: CliOpts -> Journal -> IO ()
exportalka _cliopts j = do
  let l = ledgerFromJournal Any j
  T.putStrLn $ T.toStrict $ encodeToLazyText $ toJSON l

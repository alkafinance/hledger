{-|

The @export-alka@ command exports all data as JSON compatible
with https://alka.app.

-}

-- {-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.ExportAlka (
  exportalkamode
 ,exportalka
) where

import Data.Aeson -- (toJSON)
-- import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B (toStrict)
import qualified Data.Map as M -- (toList)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Encoding as T (decodeUtf8)
-- import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.IO as T (putStrLn)
-- import qualified Data.ByteString.Lazy as BL
import           Data.Decimal
-- import           Data.Maybe
import           GHC.Generics (Generic)
import           System.FilePath (takeFileName)
import           System.Time (ClockTime) --,Day)

import Hledger  -- not including Hledger.Data.Json, instead use alka-specific JSON below
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
-- Assumes the internal data is UTF-8 encoded, will give an error otherwise.
exportalka :: CliOpts -> Journal -> IO ()
exportalka _cliopts j = do
  T.putStrLn $
    -- T.toStrict $ encodeToLazyText $  -- compact JSON
    T.decodeUtf8 $ B.toStrict $ encodePretty $  -- human-readable JSON
    object
      ["version"     .= ("1"::String)
      ,"ledgers"     .= [
          object [
             "title"        .= (takeFileName $ fst $ head $ jfiles j)
            ,"accounts"     .= (map accountNametoAlkaAccount $ journalAccountNames j)
            ,"transactions" .= jtxns j
            ]]
      ,"commodities" .= (M.elems $ jcommodities j)
      ,"prices"      .= jpricedirectives j
      ]

-- Alka-specific ToJSON instances.

-- TODO
-- should null values appear in JSON as null or undefined ?

accountNametoAlkaAccount :: AccountName -> Value
accountNametoAlkaAccount a = toJSON $ object
  ["closeDate" .= (Nothing::Maybe Bool)
  ,"openDate"  .= (Nothing::Maybe Bool)
  ,"path"      .= accountNameToAlkaAccountPath a
  ,"alias"     .= (""::String)
  ]

type AlkaAccountPath = String

accountNameToAlkaAccountPath :: AccountName -> AlkaAccountPath
accountNameToAlkaAccountPath = map (\c -> if c==':' then '/' else c) . T.unpack

instance ToJSON Status
instance ToJSON GenericSourcePos
instance ToJSON Decimal

instance ToJSON Commodity where
  toJSON Commodity{..} = object
    ["name" .= csymbol
    ,"unit" .= csymbol
    ]

-- This would duplicate Decimal's own ToJSON instance imported above.
-- instance ToJSON Quantity where
--   toJSON decimal = toJSON (realToFrac decimal :: Double)

-- Use a helper instead. Problem: how to ensure 0.005 isn't rendered as 5.0e-3 ?
decimalToFloatingPoint :: Decimal -> Double
decimalToFloatingPoint = realToFrac
-- decimalToFloatingPoint = printf "%f" realToFrac

instance ToJSON Amount where
  toJSON Amount{..} = object
    ["quantity" .= decimalToFloatingPoint aquantity
    ,"unit" .= acommodity
    ]

instance ToJSON AmountStyle
instance ToJSON Side
instance ToJSON DigitGroupStyle
instance ToJSON MixedAmount
instance ToJSON BalanceAssertion
instance ToJSON AmountPrice
instance ToJSON MarketPrice
instance ToJSON PostingType

instance ToJSON Posting where
  toJSON Posting{..} = object
    ["accountPath"       .= accountNameToAlkaAccountPath paccount
    ,"amount"            .= unifyMixedAmount pamount       -- can throw an error
    ,"cost"              .= (Nothing::Maybe Amount) -- XXX
    ,"flag"              .= showStatus pstatus
    -- ,"pcomment"          .= toJSON pcomment
    -- ,"ptype"             .= toJSON ptype
    -- ,"ptags"             .= toJSON ptags
    -- ,"pbalanceassertion" .= toJSON pbalanceassertion
    ]

instance ToJSON Transaction where
  toJSON t@Transaction{..} = object
    ["date"  .= tdate
    ,"flag"  .= showStatus tstatus
    ,"links" .= ([]::[String])
    ,"tagNames" .= map fst ttags
    ,"payee" .= transactionPayee t
    ,"notes" .= transactionNote t
    ,"postings" .= tpostings
    ]                                    


instance ToJSON TransactionModifier
instance ToJSON PeriodicTransaction

instance ToJSON PriceDirective where
  toJSON PriceDirective{..} = object
    ["baseUnit"   .= pdcommodity
    ,"targetUnit" .= acommodity pdamount
    ,"rate"       .= (decimalToFloatingPoint $ aquantity pdamount)
    ,"date"       .= pddate
    ]                                    
  
instance ToJSON DateSpan
instance ToJSON Interval
instance ToJSON AccountAlias
instance ToJSON AccountType
instance ToJSONKey AccountType
instance ToJSON AccountDeclarationInfo
instance ToJSON TimeclockCode
instance ToJSON TimeclockEntry
instance ToJSON ClockTime
instance ToJSON Journal

instance ToJSON Account where
  toJSON a = object
    ["aname"        .= aname a
    ,"aebalance"    .= aebalance a
    ,"aibalance"    .= aibalance a
    ,"anumpostings" .= anumpostings a
    ,"aboring"      .= aboring a
    -- To avoid a cycle, show just the parent account's name
    -- in a dummy field. When re-parsed, there will be no parent.
    ,"aparent_"     .= (maybe "" aname $ aparent a)
    -- Just the names of subaccounts, as a dummy field, ignored when parsed.
    ,"asubs_"       .= (map aname $ asubs a)
    -- The actual subaccounts (and their subs..), making a (probably highly redundant) tree
    -- ,"asubs"        .= toJSON (asubs a)
    -- Omit the actual subaccounts
    ,"asubs"        .= ([]::[Account])
    ]

deriving instance Generic (Ledger)
instance ToJSON Ledger

-- From JSON

instance FromJSON Status
instance FromJSON GenericSourcePos
instance FromJSON Amount
instance FromJSON AmountStyle
instance FromJSON Side
instance FromJSON DigitGroupStyle
instance FromJSON MixedAmount
instance FromJSON BalanceAssertion
instance FromJSON AmountPrice
instance FromJSON MarketPrice
instance FromJSON PostingType
instance FromJSON Posting
instance FromJSON Transaction
instance FromJSON AccountDeclarationInfo
-- XXX The ToJSON instance replaces subaccounts with just names.
-- Here we should try to make use of those to reconstruct the
-- parent-child relationships.
instance FromJSON Account

-- Decimal, various attempts
--
-- https://stackoverflow.com/questions/40331851/haskell-data-decimal-as-aeson-type
----instance FromJSON Decimal where parseJSON =
----  A.withScientific "Decimal" (return . right . eitherFromRational . toRational)
--
-- https://github.com/bos/aeson/issues/474
-- http://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson-TH.html
-- $(deriveFromJSON defaultOptions ''Decimal) -- doesn't work
-- $(deriveFromJSON defaultOptions ''DecimalRaw)  -- works; requires TH, but gives better parse error messages
--
-- https://github.com/PaulJohnson/Haskell-Decimal/issues/6
--deriving instance Generic Decimal
--instance FromJSON Decimal
deriving instance Generic (DecimalRaw a)
instance FromJSON (DecimalRaw Integer)
--
-- @simonmichael, I think the code in your first comment should work if it compiles—though “work” doesn’t mean you can parse a JSON number directly into a `Decimal` using the generic instance, as you’ve discovered.
--
--Error messages with these extensions are always rather cryptic, but I’d prefer them to Template Haskell. Typically you’ll want to start by getting a generic `ToJSON` instance working, then use that to figure out what the `FromJSON` instance expects to parse: for a correct instance, `encode` and `decode` should give you an isomorphism between your type and a subset of `Bytestring` (up to the `Maybe` wrapper that `decode` returns).
--
--I don’t have time to test it right now, but I think it will also work without `DeriveAnyClass`, just using `DeriveGeneric` and `StandAloneDeriving`. It should also work to use the [`genericParseJSON`](http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:genericParseJSON) function to implement the class explicitly, something like this:
--
--{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE StandAloneDeriving #-}
--import GHC.Generics
--import Data.Aeson
--deriving instance Generic Decimal
--instance FromJSON Decimal where
--  parseJSON = genericParseJSON defaultOptions
--
--And of course you can avoid `StandAloneDeriving` entirely if you’re willing to wrap `Decimal` in your own `newtype`.


-- Utilities

-- -- | Read a json from a file and decode/parse it as the target type, if we can.
-- -- Example:
-- -- >>> readJsonFile "in.json" :: IO MixedAmount
-- readJsonFile :: FromJSON a => FilePath -> IO a
-- readJsonFile f = do
--   bs <- BL.readFile f
--   let v = fromMaybe (error "could not decode bytestring as json value") (decode bs :: Maybe Value)
--   case fromJSON v :: FromJSON a => Result a of
--     Error e   -> error e
--     Success t -> return t

-- -- | Write some to-JSON-convertible haskell value to a json file, if we can.
-- -- Example:
-- -- >>> writeJsonFile "out.json" nullmixedamt
-- writeJsonFile :: ToJSON a => FilePath -> a -> IO ()
-- writeJsonFile f v = BL.writeFile f (encode $ toJSON v)


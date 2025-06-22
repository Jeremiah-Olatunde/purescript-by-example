module Test.MySolutions where

import Prelude

import Data.AddressBook (AddressBook, Entry)
import Data.List (filter, head)
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter hasMatchingAddress
  where
  hasMatchingAddress :: Entry -> Boolean
  hasMatchingAddress = (eq street) <<< _.address.street


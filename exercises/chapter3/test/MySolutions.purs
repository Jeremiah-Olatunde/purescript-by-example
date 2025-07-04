module Test.MySolutions where

import Prelude

import Data.AddressBook (AddressBook, Entry)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter hasMatchingAddress
  where
  hasMatchingAddress :: Entry -> Boolean
  hasMatchingAddress = (eq street) <<< _.address.street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not <<< null <<< filter hasName
  where
  hasName :: Entry -> Boolean
  hasName entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq equal
  where
  equal :: Entry -> Entry -> Boolean
  equal a b = a.firstName == b.firstName && a.lastName == b.lastName

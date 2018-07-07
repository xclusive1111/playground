module Data.AddressBook where
import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe)


type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry e = e.lastName <> ", " <>
              e.firstName <> ", " <>
              showAddress e.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = Cons entry book

filterEntry' :: String -> String -> (Entry -> Boolean)
filterEntry' firstName lastName = \e -> e.firstName == firstName && e.lastName == lastName
  
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry e = e.firstName == firstName && e.lastName == lastName

findEntry' :: String -> String -> AddressBook -> Maybe Entry
findEntry' firstName lastName book = head $ filter (filterEntry' firstName lastName) book

-- using backward composition and eta reduction
findEntry'' :: String -> String -> AddressBook -> Maybe Entry
findEntry'' firstName lastName = head <<< filter (filterEntry' firstName lastName)

removeDuplidates :: AddressBook -> AddressBook
removeDuplidates = nubBy (\e1 e2 -> e1.firstName == e2.firstName && e1.lastName == e2.lastName)

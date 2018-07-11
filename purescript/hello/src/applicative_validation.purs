module Data.Validator where

import Prelude
import Data.AddressBook
import Control.Apply
import Control.Applicative
import Data.Either
import Data.Maybe
import Data.List
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

newtype Address = Address
  { street :: String
  , city   :: String
  , state  :: String
  }

address :: String -> String -> String -> Address
address a b c = Address { street: a, city: b, state: c }

type FullName = String
type ErrorStr = String

fullName :: String -> String -> String -> FullName
fullName first middle last = last <> ", " <> first <> " " <> middle

withError :: forall a b. Maybe a -> b -> Either b a
withError (Just a)  _   = Right a
withError Nothing error = Left error

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either ErrorStr FullName
fullNameEither first middle last =
  fullName <$> (first  `withError` "First name was missing")
           <*> (middle `withError` "Middle name was missing")
           <*> (last   `withError` "Last name was missing")

missingLastName = fullNameEither (Just "First") (Just "Last") Nothing
missingFirstName = fullNameEither Nothing (Just "Last") Nothing

combineList :: forall f a. Applicative f => List (f a) -> f (List a)
combineList Nil         = pure Nil
combineList (Cons fa xs) = Cons <$> fa <*> combineList xs

data PhoneType = HomePhone | WorkPhone | CellPhone | OtherPhone

newtype PhoneNumber = PhoneNumber
  { "type" :: PhoneType
  , number :: String
  }

newtype Person = Person
  { firstName   :: String
  , lastName    :: String
  , homeAddress :: Address
  , phones      :: Array PhoneNumber
  }

instance showAddress :: Show Address where
  show (Address a) = showAddress a

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber pt number = PhoneNumber { "type": pt, number: number }

person :: String -> String -> Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones =
  Person {firstName, lastName, homeAddress, phones}


examplePerson :: Person
examplePerson = person 
  "Kid" "Love" 
  (address "123 Fake addr" "Some Town" "Some State")
  [ phoneNumber HomePhone "111-222-333"
  , phoneNumber CellPhone "222-111-333"
  ]

nonEmpty :: String -> Either String Unit
nonEmpty "" = Left "Field cannot be empty"
nonEmpty _  = Right unit

validatePerson :: Person -> Either String Person
validatePerson (Person p) =
  person <$> (nonEmpty p.firstName *> pure p.firstName)
         <*> (nonEmpty p.lastName  *> pure p.lastName)
         <*> pure p.homeAddress
         <*> pure p.phones

-- Validation using Semigroup
type Errors = Array String

nonEmpty' :: String -> String -> V Errors Unit
nonEmpty' field "" = invalid ["Field '" <> field <> "' cannot be empty'"]
nonEmpty' _     _  = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len =
                         invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     = pure unit

validateAddress :: Address -> V Errors Address
validateAddress (Address a) =
  address <$> (nonEmpty' "Street"   a.street *> pure a.street)
          <*> (nonEmpty' "City"     a.city   *> pure a.city)
          <*> (lengthIs  "State" 2  a.state  *> pure a.state)


invalidAddr = validateAddress $ address "" "" ""

-- Validation with Regular expression

matches :: String -> Regex -> String -> V Errors Unit
matches _     regex  value | test regex value = pure unit
matches field _      _     = invalid ["Field '" <> field <> "' did not match the required format"]

phoneNumberRegex :: Regex
phoneNumberRegex = unsafePartial 
  case regex "^\\d{3}-\\d{3}-\\d{3}" noFlags
    of (Right r) -> r

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber p) =
  phoneNumber <$> pure p."type"
              <*> (matches "Number" phoneNumberRegex p.number *> pure p.number)


-- Traversable Functors

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid ["Field '" <> "' must contain at least one value"]
arrayNonEmpty _     _  = pure unit

validatePerson' :: Person -> V Errors Person
validatePerson' (Person p) =
  person <$> (nonEmpty' "First Name" p.firstName *> pure p.firstName)
         <*> (nonEmpty' "Last Name"  p.lastName  *> pure p.lastName)
         <*> validateAddress p.homeAddress
         <*> (arrayNonEmpty "Phone Number" p.phones *> traverse validatePhoneNumber p.phones)

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards #-}
module Interview where

import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Maybe
import           Control.Monad.IO.Class

-- * Advances
--
-- When our users run out of money before their paycheck, we send them some money as an Advance. 
-- Each Advance has a due date, which is set to the next date of their regular paycheck. 
-- We need to collect these advances, but we never want to cause an overdraft by doing so.
-- Assume advances are less than $100


-- * Requirements
--
-- On the due date of an advance, we want to collect it immediately if they have a balance of $100 or more
-- If their balance is lower than $100 on the due date, we collect it as soon as any credit occurs in their bank account greater than the amount of the advance
-- If 15 days pass without a sufficient credit happening, we put their account on hold.
-- We must not collect the same advance twice


-- * Assignemnt
--
-- Change and add types to effectively model the data
-- Write functions that meet the requirements
-- How and when should your functions be called?
-- Add annotations to all functions. This module should compile, but you won't be able to run it because the resources (Store, Bank) don't have implementations.

-- Bonus: How can `Store` and `Id` be improved so you can't pass the wrong identifer to storeFind? (Compiler error if you try to find an Account with an Advance's Id)

-- Discussion: How would you write tests for this code?
-- Discussion: What would you change about the effects below (Store, Bank)?


data TODO = TODO

collectAdvances :: (Monad m, Bank m, Store Advance m) => m ()
collectAdvances = do
  now                  <- liftIO getCurrentTime
  allAdvs :: [Advance] <- storeLoad
  let dueAdvance = filterDueAdvances now allAdvs
  pure ()


data DueAdvance =
    OverDue     Advance
  | FirstDayDue Advance
  | Due         Advance

-- this function will filter get a lsit of advances and filter them if they are due
-- and based on the time of the due andvance it will turn them to DueAdvance type
filterDueAdvances :: UTCTime -> [Advance] -> [DueAdvance]
filterDueAdvances now advs = mapMaybe filterDueAdvance advs
 where
  filterDueAdvance :: Advance -> Maybe DueAdvance
  filterDueAdvance adv@Advance {..}
    | utctDay advanceDueDate == utctDay now = Just $ FirstDayDue adv
    | addDays 15 (utctDay advanceDueDate) <= utctDay now = Just $ Due adv
    | addDays 15 (utctDay advanceDueDate) > utctDay now = Just $ Due adv
    | otherwise                             = Nothing

advanceAction :: (Monad m, Bank m, Store Advance m) => DueAdvance -> m ()
advanceAction du = case du of
  FirstDayDue adv -> firstDayAction adv
  OverDue     adv -> overDueAction adv
  Due         adv -> dueAction adv
 where
  firstDayAction adv = undefined
  overDueAction adv = undefined
  dueAction adv = undefined

-- example :: Bank m => AccountId -> m ()
-- example accountId = do
--   ts  <- bankTransactions accountId
--   bal <- bankBalance accountId
--   bankCredit accountId 120.45

otherFunctions :: TODO
otherFunctions = TODO


-- * Resources
--
-- You can use the effects and types below. Feel free to modify them.

-- Scheduler -----------------------------------
-- You have access to a scheduler, which can run your function on an interval
-- Daily -> your function will be passed Data.Time.Calendar.Day
-- Hourly -> your function will be passed Data.Time.Clock.UTCTime

-- > exampleDaily :: Store Advance m => Day -> m ()
-- > exampleDaily day = do
-- >   as <- storeLoad
-- >   -- do something with the advances using the date
-- >   pure ()



-- Accounts ------------------------------------
-- Accounts can be active or on hold

-- Advances are associated with an account, have a due date, and an amount
-- Advances are "due" once it is the due date or later
-- Advances are "collected" 

type AccountId = String
data AccountStatus = Active | OnHold
data Account = Account {
    aId :: AccountId
  , aStatus :: AccountStatus
  }


type AdvanceId = String
data Advance = Advance {
    advanceAmount :: Amount
  , advanceDate :: UTCTime
  , advanceDueDate :: UTCTime
  , accountId :: AccountId
}





-- | Bank -----------------------------------------
-- Transactions are associated with an Account
-- Transactions have a date, an amount, and are either a credit or debit
-- You can send ACH credits (give them money) and debits (take money)
-- You can check their bank balance
-- You can get all their transactions

-- > example :: Bank m => AccountId -> m ()
-- > example accountId = do
-- >   ts <- bankTransactions accountId
-- >   bal <- bankBalance accountId
-- >   bankCredit accountId 120.45

class Monad m => Bank m where
  bankCredit       :: AccountId -> Amount -> m ()
  bankDebit        :: AccountId -> Amount -> m ()
  bankBalance      :: AccountId -> m Amount
  bankTransactions :: AccountId -> m [Transaction]

type Amount = Float
data Transaction = Transaction{
    transactionAmoung :: Amount
  , transactionTime :: UTCTime
}


-- | Store: save and load records ------------------
--
-- > example :: Store Advance m => m ()
-- > example = do
-- >   let adv = Advance
-- >   storeSave adv


type Id = String
class Monad m => Store a m where
  storeSave :: a -> m ()
  storeFind :: Id -> m (Maybe a)
  storeLoad :: m [a]





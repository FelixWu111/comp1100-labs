-- File/Module name: Dates (.hs)
-- Author: <Yu Wu>, u<u6250866>
-- Date: <10.5.2018>
-- Description: Provides types, names and functions for dates.
module Dates
  ( Date(Date, day', month', year')
  , Day
  , Month
  , Year
  , Days(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
     Sunday)
  , Months(January, February, March, April, May, June, July, August,
       September, October, November, December)
  , iso_day_no_to_name -- :: Positive -> Days
  , day_name_to_iso_day_no -- :: Days -> Positive
  ) where

import Integer_Subtypes
data Days
 = Monday
 | Tuesday
 | Wednesday
 | Thursday
 | Friday
 | Saturday
 | Sunday
 deriving (Eq, Enum, Show)

data Months
 = January
 | February
 | March
 | April
 | May
 | June
 | July
 | August
 | September
 | October
 | November
 | December
 deriving (Eq, Enum, Show)

-- Synonymous types to make function signatures more readable
type Day = Positive -- 1..31

type Month = Months

type Year = Natural

data Date = Date
  { day' :: Day
  , month' :: Month
  , year' :: Year
  } deriving (Eq, Show)

-- Convert between ISO day numbers (Monday = 1, .., Sunday = 7) and day names
iso_day_no_to_name :: Positive -> Days
iso_day_no_to_name iso_day_no = toEnum (fromInteger (toInteger iso_day_no - 1))

day_name_to_iso_day_no :: Days -> Positive
day_name_to_iso_day_no day = fromInteger (1 + toInteger (fromEnum day))
-- File name: Day_Of_Week.hs
-- Author: <Yu Wu>, u<u6250866>
-- Date: <10.5.2018>
-- Description: Provides functions to assist in calculating
--              the day of the week.
module Day_Of_Week where

import Dates
import Integer_Subtypes
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

-- | Compute the number of days since 1 January 0000.
-- >>> days_since_1_January_0 (Date 1 January 0000)
-- 0
-- >>> days_since_1_January_0 (Date 1 January 0001)
-- 366
-- >>> days_since_1_January_0 (Date 1 January 0004)
-- 1461
-- >>> days_since_1_January_0 (Date 1 March 0004)
-- 1521
-- >>> days_since_1_January_0 (Date 1 January 0005)
-- 1827
-- >>> days_since_1_January_0 (Date 1 March 0005)
-- 1886
-- >>> days_since_1_January_0 (Date 9 October 1993)
-- 728210
days_since_1_January_0 :: Date -> Natural
days_since_1_January_0 (Date day month year) =
  previous_days_this_month day +
  days_in_previous_months month year +
  days_before_this_year year

previous_days_this_month :: Day -> Natural
previous_days_this_month day = from_Positive_to_Natural day - 1

days_in_previous_months :: Month -> Year -> Natural
days_in_previous_months month year =
  sum (take (fromEnum month) (month_lengths year))
  where
    month_lengths :: Year -> [Natural]
    month_lengths year = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    feb
      | is_leap_year year = 29
      | otherwise = 28

is_leap_year :: Year -> Bool
is_leap_year year = mod year 4 == 0 && (mod year 100 /= 0 || mod year 400 == 0)

days_before_this_year :: Year ->Natural
days_before_this_year year = (leap_years_since_1_January_0 year*366) + (year - leap_years_since_1_January_0 year)*365

leap_years_since_1_January_0 :: Year -> Natural
leap_years_since_1_January_0 year = case year of
    0 -> 0
    _ -> 1 + pre_year `quot` 4 - pre_year `quot` 100 + pre_year `quot` 400
        where pre_year = year - 1

day_of_week :: Date -> Days
day_of_week date = iso_day_no_to_name $ from_Natural_to_Positive (mod (days_since_1_January_0 date) 7) - 1

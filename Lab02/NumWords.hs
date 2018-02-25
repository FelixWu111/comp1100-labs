--
-- The main function is convertToWords.
-- Try for instance: convertToWords 1235543222355643
--
-- Instead of a list of words, you can also produce a single string with
-- convertToString.
--
-- Adapted from Uwe Zimmer's 2012 adaptation of the problem from Introduction
-- to Functional Programming, by Richard Bird & Philip Wadler
--
module NumWords
  ( Words
  , NumberInWords
  , convertToWords -- :: Integer -> NumberInWords
  , convertToString -- :: Integer -> String
  ) where

data Words
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Eleven
  | Twelve
  | Thirteen
  | Fourteen
  | Fifteen
  | Sixteen
  | Seventeen
  | Eighteen
  | Nineteen
  | Twenty
  | Thirty
  | Forty
  | Fifty
  | Sixty
  | Seventy
  | Eighty
  | Ninety
  | Hundred
  | Thousand
  | Million
  | Billion
  | Trillion
  | Quadrillion
  | Quintillion
  | Sextillion
  | Septillion
  | And
  | Comma
  | Minus
  deriving (Show)

type NumberInWords = [Words]

toString :: NumberInWords -> String
toString number =
  case number of
    [] -> ""
    n:ns -> show n ++ toString ns

digits :: Integer -> Words
digits x =
  case x of
    0 -> Zero
    1 -> One
    2 -> Two
    3 -> Three
    4 -> Four
    5 -> Five
    6 -> Six
    7 -> Seven
    8 -> Eight
    9 -> Nine
    _ -> error "no digit for given index"

xTeen :: Integer -> Words
xTeen x =
  case x of
    0 -> Ten
    1 -> Eleven
    2 -> Twelve
    3 -> Thirteen
    4 -> Fourteen
    5 -> Fifteen
    6 -> Sixteen
    7 -> Seventeen
    8 -> Eighteen
    9 -> Nineteen
    _ -> error "no teen for given index"

xTy :: Integer -> Words
xTy x =
  case x of
    2 -> Twenty
    3 -> Thirty
    4 -> Forty
    5 -> Fifty
    6 -> Sixty
    7 -> Seventy
    8 -> Eighty
    9 -> Ninety
    _ -> error "no xTy for given index"

orderTerm :: Integer -> Words
orderTerm order =
  case order of
    3 -> Thousand
    6 -> Million
    9 -> Billion
    12 -> Trillion
    15 -> Quadrillion
    18 -> Quintillion
    21 -> Sextillion
    24 -> Septillion
    _ -> error "No order term for given order"

maxOrder = 24 :: Integer

splitOrder :: Integer -> Integer -> (Integer, Integer)
splitOrder order n = (n `div` 10 ^ order, n `mod` 10 ^ order)

convertTens :: Integer -> NumberInWords
convertTens n =
  case splitOrder 1 n of
    (0, digit) -> [digits digit]
    (1, digit) -> [xTeen digit]
    (tens, 0) -> [xTy tens]
    (tens, digit) -> (xTy tens) : [digits digit]

convertHundreds :: Integer -> NumberInWords
convertHundreds n =
  case splitOrder 2 n of
    (0, tens) -> convertTens tens
    (hundreds, 0) -> (digits hundreds) : [Hundred]
    (hundreds, tens) -> (digits hundreds) : (Hundred : (And : convertTens tens))

link :: Integer -> Words
link rest
  | rest < 100 = And
  | otherwise = Comma

convertOrders :: Integer -> Integer -> NumberInWords
convertOrders order n =
  case order of
    0 -> convertHundreds n
    _ ->
      case splitOrder order n of
        (0, rest) -> convertOrders (order - 3) rest
        (num, 0) -> convertHundreds num ++ [orderTerm order]
        (num, rest) ->
          convertHundreds num ++
          (orderTerm order : (link rest : convertOrders (order - 3) rest))

convertToWords :: Integer -> NumberInWords
convertToWords n
  | n >= 10 ^ (maxOrder + 3) =
    error
      "Integer out of range for function: convertToWords (don't know any more order terms)"
  | n < 0 = Minus : convertOrders maxOrder (abs n)
  | n >= 0 = convertOrders maxOrder n
  | otherwise =
    error "Program error: Non-exhaustive guards in function: convertToWords"

convertToString :: Integer -> String
convertToString n = toString (convertToWords n)

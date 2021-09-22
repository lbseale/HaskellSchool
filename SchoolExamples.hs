{-# LANGUAGE OverloadedStrings #-}

import Data.Text ( Text )

-- ********** Control Structures **********

-- ***** If *****

-- Is this integer even or odd?
evenOrOdd :: Int -> Text
evenOrOdd val = 
    if val `mod` 2 == 0 then "Value is even" else "Value is odd"

-- How to navigate a traffic light
trafficLight :: Text -> Text
trafficLight lightColor = 
    if (lightColor == "Red" || lightColor == "Yellow")
    then "Slown down"
    else "Roll through"

-- Give a nice description for the weather
-- Note how `warmOrCol` and `niceOrGloomy` are defined in a let block then 
-- used in the final definition
weatherDescription :: Text -> Int -> Text
weatherDescription weather tempInC = let
    warmOrCold   = if tempInC > 21       then "It's warm" else "It's cold"
    niceOrGloomy = if weather == "sunny" then "nice out"  else "gloomy out"
    in
    warmOrCold <> " and " <> niceOrGloomy

-- ***** Case *****

-- Offer some helpful advice, given the weather
weatherAdvice :: Text -> Text
weatherAdvice weather = 
    case weather of
        "fair"     -> "It'll be nice today"
        "rainy"    -> "Bring an umbrella"
        "sunny"    -> "Wear sunscreen"
        "freezing" -> "You'll need a coat"
        _          -> "Not sure"

-- ***** Guard *****

-- Subjective descriptions of temperature, after converting to Celcius
-- Note how `tmpInC` is defined in the `where` clause
subjectiveTemp :: Double -> Text
subjectiveTemp tmpInFaranheit
    | tmpInC <= 0.0                   = "Freezing"
    | tmpInC > 0.0  && tmpInC <= 20.0 = "Cold"
    | tmpInC > 20.0 && tmpInC <= 30.0 = "Comfortable"
    | tmpInC > 30.0                   = "Hot"
    | otherwise                       = "Not Sure"
  where
    tmpInC = (tmpInFaranheit - 32.0 ) * ( 5.0 / 9.0 )

-- Clothing Sizer
-- Note that `yourSize` is defined in a where clause, then used in the
-- definition
whatSize :: Int -> Text
whatSize heightInCm = "You're a size " <> yourSize
  where
    yourSize
        | heightInCm > 0   && heightInCm <= 166 = " Small"
        | heightInCm > 166 && heightInCm <= 178 = " Medium"
        | heightInCm > 178                      = " Large"
        | otherwise                             = " Not sure"

-- State which value is bigger
biggerOrSmaller :: Int -> Int -> Text
biggerOrSmaller val1 val2
    | val1 > val2 = "Value 1 is bigger"
    | val2 > val1 = "Value 2 is bigger"
    | otherwise   = "Both values are equal"

badGuard :: Text -> Text
badGuard txtIn
    | txtIn == "Ok" = "It worked"


safeGuard :: Text -> Text
safeGuard txtIn
    | txtIn == "Ok" = "It worked"
    | otherwise = "It still worked"

-- ********** Pattern Matching **********

-- import Data.Text ( append )

-- ***** Enum type *****

data Suit = Hearts | Diamonds | Clubs | Spades
--     deriving Eq

-- Read out the color of the suit
suitColor :: Suit -> Text
suitColor Hearts   = "Red"
suitColor Diamonds = "Red"
suitColor Clubs    = "Black"
suitColor Spades   = "Black"

-- Same as above, but use a guard instead of pattern matching
-- suitColorWithGuard :: Suit -> Text
-- suitColorWithGuard suit
--     | suit == Hearts || suit == Diamonds = "Red"
--     | otherwise = "Black"

-- ***** Lists *****

-- How big is a list?
-- Danger! Incomplete patterns!
-- If you compile with `-Wall`, or use `:set -Wall` in GHCI you will get a
-- warning about this
listSize1 :: [Text] -> Text
listSize1 []      = "Empty"
listSize1 [elem1] = "One element: " <> elem1

-- How big is a list?
-- Using the `_` pattern is a catch-all for any value
listSizeCatch :: [Text] -> Text
listSizeCatch []      = "Empty"
listSizeCatch [elem1] = "One element: " <> elem1
listSizeCatch _       = "More than one element"

-- How big is a list?
-- Notice how the list constructor `:` is used
-- In the `(elem1:[])` pattern, it constructs the value with an empty list `[]`
-- This indicates a single-element list
-- In the `(elem1:_)` pattern, it constructs the value with a list of any 
-- length
listSizeCons :: [Text] -> Text
listSizeCons []      = "Empty"
listSizeCons (elem1:[]) = "One element: " <> elem1
listSizeCons (elem1:_)  = "More than one element, starting with: " <> elem1

-- ***** Maybe *****

-- Note how the pattern-match offers a way to deconstruct a `Maybe` value
maybeToText :: Maybe Text -> Text
maybeToText Nothing = "No text! Nothing!"
maybeToText (Just t) = "The text is: " <> t

-- Pattern matches also work in `case` statements
maybeWithCase :: Maybe Text -> Text
maybeWithCase mText = let
    textFound = case mText of
        Nothing -> "No text"
        (Just t) -> t
    in
    "I found: " <> textFound

-- ***** Either *****

-- Pattern matching is common for deconstructing Either values
eitherToText :: Either Text Text -> Text
eitherToText (Left errMsg) = "Error: " <> errMsg
eitherToText (Right okMsg) = "Success: " <> okMsg


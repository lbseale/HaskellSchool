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
weatherAdvice weatherDescription = 
    case weatherDescription of
        "fair"     -> "It'll be nice today"
        "rainy"    -> "Bring an umbrella"
        "sunny"    -> "Wear sunscreen"
        "freezing" -> "You'll need a coat"
        otherwise  -> "Not sure"

-- ***** Guard *****

-- Subjective descriptions of temperature
subjectiveTemp :: Int -> Text
subjectiveTemp tmpInC
    | tmpInC <= 0                  = "Freezing"
    | tmpInC > 0   && tmpInC <= 20 = "Cold"
    | tmpInC > 20  && tmpInC <= 30 = "Comfortable"
    | tmpInC > 30                  = "Hot"


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

---
version: 1.0.0
title: Control Structures
---

Recall that Haskell functions are *definitions*, not lists of instructions

In imperative programming, control structures are used to decide which block of
code should be executed

In Haskell, they are used instead to decide how a certain value should be
defined

__Note__: Usually control structures are used directly in the definition of a 
function, but they can also be used in the definition of any intermediate
value inside a function (we will see examples of both)

Let's have a look at the control structures available in Haskell, and some
examples of how they work

{% include toc.html %}

## Preamble

You will need to add the following lines to the top of your Haskell source 
file (`.hs`) to be able to run these examples

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text ( Text )
```

## If Statements

Haskell provides a simple set of keywords for `if` statments, with only a 
single `else` condition

```haskell
-- Is this integer even or odd?
evenOrOdd :: Int -> Text
evenOrOdd val = 
    if val `mod` 2 == 0 then "Value is even" else "Value is odd"

-- How to navigate a traffic light
trafficLight :: Text -> Text
trafficLight lightColor = 
    if (lightColor == "Red" || lightColor == "Yellow")
    then "Slow down"
    else "Roll through"

-- Give a nice description for the weather
-- Note how `warmOrCold` and `niceOrGloomy` are defined in a let block then 
-- used in the final definition
weatherDescription :: Text -> Int -> Text
weatherDescription weather tempInC = let
    warmOrCold   = if tempInC > 21       then "It's warm" else "It's cold"
    niceOrGloomy = if weather == "sunny" then "nice out"  else "gloomy out"
    in
    warmOrCold <> " and " <> niceOrGloomy
```

`if` statements are not the most common control structure 

They are handy if you have a simple definition, and don't need one of the more
flexible structures we will see soon

## Case Statements

Case statements offer a way to switch on any one of a number of possible values

```haskell
-- Offer some helpful advice, given the weather
weatherAdvice :: Text -> Text
weatherAdvice weatherDescription = 
    case weatherDescription of
        "fair"     -> "It'll be nice today"
        "rainy"    -> "Bring an umbrella"
        "sunny"    -> "Wear sunscreen"
        "freezing" -> "You'll need a coat"
        otherwise  -> "Not sure"
```

Case statements use a feature called *Pattern Matching* to decide which case to
choose; we will explore this further in the next chapter

## Guards

Guards allow you to choose between one of many possible conditions; they are
similar to an `if`-`else if` structure

When using guards to define a value, the `=` sign is omitted

```haskell
-- Subjective descriptions of temperature
-- Note the lack of an `=` sign after the function name
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
```


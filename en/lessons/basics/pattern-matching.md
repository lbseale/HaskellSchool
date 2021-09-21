---
version: 1.0.0
title: Pattern Matching
---

Haskell offers a powerful type-based system for deciding function definitions
called Pattern Matching

In general, Pattern Matching is used to deconstruct algebraic types, let's see
some examples to learn how it works

{% include toc.html %}

## Pragmas and Imports

You will need to add the following lines to the top of your Haskell source 
file (`.hs`) to be able to run these examples

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text ( Text )
```

## Enumerated Types

Say we have a type for the four suits of playing cards

```haskell
data Suit = Hearts | Diamonds | Clubs | Spades
```

### Case Statement

Let's write a `case` statement which will read the color of the suit

```haskell
-- Show the color of the suit
suitColorCase :: Suit -> Text
suitColorCase suitIn = case suitIn of
     Hearts   -> "Red"
     Diamonds -> "Red"
     Clubs    -> "Black"
     Spades   -> "Black"
```

The `case` statement is using Pattern Matching to decide which text to return

The value of `suitIn` is inspected, and then checked against the given patterns
in the `case` statement
- In this example, each pattern is just a data constructor
- We will get to see more sophisticated patterns a bit later

### Pattern

It turns out that we can write the same function more simply

```haskell
-- Show the color of the suit
suitColor :: Suit -> Text
suitColor Hearts   = "Red"
suitColor Diamonds = "Red"
suitColor Clubs    = "Black"
suitColor Spades   = "Black"
```

Here we have build the patterns into the function definition, without using a
`case` statment

The function works the same way: any input value is inspected and matched
with the different patterns provided for the function definition

In truth, the `case` statement is redundant in the first example, and as
Haskell programmers we like to keep our code as concise as is reasonable

This can feel awkward at first, since in imperative languages there is usually
only a single declaration of a function
- In Haskell, this type of syntax is common
- Always keep in mind that Haskell functions are definitions, and with Pattern
  Matching we are providing different definitions for different inputs

## Lists

Pattern Matching can be used on more sophisticated patterns then just plain data
constructors.

Using Pattern Matching to handle lists is very common, let's see some examples

### First Try

```haskell
-- How big is a list?
listSize1 :: [Text] -> Text
listSize1 []      = "Empty"
listSize1 [elem1] = "One element: " <> elem1
```

This is a dangerous function! Let's discover why
- The first pattern `[]` will match an empty list
- The second pattern `[elem1]` will match a single-element list
- What happens the list has more than one element?

A runtime error happens! This is an appalling to a Haskell developer; the whole
point of using Haskell is to catch issues like this at compile time

__Note__: If you compile with `-Wall` or use `:set -Wall` in GHCi, this code
will not compile. You'll get this warning:

```console?lang=haskell&prompt=ghci>,ghci|
    Pattern match(es) are non-exhaustive
    In an equation for ‘listSize1’: Patterns not matched: (_:_:_)
   |
92 | listSize1 [] = "Empty"
   | ^^^^^^^^^^^^^^^^^^^^^^...
```

This is why `-Wall` is recommended; nobody wants a nasty surprise when they run
their code

### A Safer Choice

Here is a safe example of list Pattern Matching

```haskell
-- How big is a list?
-- Using the `_` pattern is a catch-all for any value
listSizeCatch :: [Text] -> Text
listSizeCatch []      = "Empty"
listSizeCatch [elem1] = "One element: " <> elem1
listSizeCatch _       = "More than one element"
```

The `_` pattern matches everything, so we know this function will work no matter
what we pass it

### Matching With Constructors

We can use any data constructor in patterns, including the list constructor `:`

```haskell
-- How big is a list?
listSizeCons :: [Text] -> Text
listSizeCons []      = "Empty"
listSizeCons (elem1:[]) = "One element: " <> elem1
listSizeCons (elem1:_)  = "More than one element, starting with: " <> elem1
```

Notice how the list constructor `:` is used in the patterns
- In the `(elem1:[])` pattern, it constructs the value with an empty list `[]`
  - This will only match a single-element list
  - It is the same as writing `[elem1]`
- In the `(elem1:_)` pattern, it constructs the value with a the catch-all
  symbol `_`
  - So this pattern will match any list with one or more elemens
  - This pattern would match a single-element list, but it won't since it is
    listed *after* single-element pattern

Order matters! The first matching pattern will be used, even if later patterns
would also match

__Note__: Parentheses `()` are required to write patterns with constructors

## Maybe

### Pattern Matching in Function Definition

Pattern Matching provides a nice way to deconstruct `Maybe` values

```haskell
maybeToText :: Maybe Text -> Text
maybeToText Nothing = "No text! Nothing!"
maybeToText (Just t) = "The text is: " <> t
```

Notice how the value `t` can be used if the `(Just t)` pattern is matched; of
course this value doesn't exist if the `Maybe` value is `Nothing`

Just as with lists, the parentheses `()` are necessary when we are matching the
`Just` constructor

### Pattern Matching in an Intermediate Value

Let's use Pattern Matching in a `case` statemen to define an intermediate value

```haskell
maybeWithCase :: Maybe Text -> Text
maybeWithCase mText = let
    textFound = case mText of
        Nothing -> "No text"
        (Just t) -> t
    in
    "I found: " <> textFound
```

## Either

Pattern matching is common for deconstructing Either values

```haskell
eitherToText :: Either Text Text -> Text
eitherToText (Left errMsg) = "Error: " <> errMsg
eitherToText (Right okMsg) = "Success: " <> okMsg
```




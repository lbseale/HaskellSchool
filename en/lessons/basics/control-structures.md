---
version: 1.0.0
title: Control Structures
---

Haskell functions are definitions, not lists of instructions. 
So instead of controlling the execution order of your program, control structures affect how results are defined. 
It can take some time to get accustomed to this difference, but once you are they will feel natural.

Note that all of the structures can be used directly in the definition of a function, 
as well as in the definition of any intermediate value. We will see examples of both.

Let's have a look at the control structures available in Haskell, and some examples of how they work.

{% include toc.html %}

## If Statements

Haskell provides a simple set of keywords for `if` statments.
There is only a single `else` condition.

```Haskell
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
```

`if` statements are not the most common control structure. 
They are handy if you have a simple definition, and don't need one of the more
flexible structures we will see soon

## Case Statements

Case statements offer a way to switch on any one of a number of possible values.

```Haskell
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

Case statements use a feature called Pattern Matching to decide which case to
choose, we will explore this further in the next chapter

## Guards

Guards allow you to choose between one of many possible conditions. They are
similar to an if-else if structure

```Haskell
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
```





## Old Elixir Stuff

If it's necessary to match against multiple patterns we can use `case/2`:

```elixir
iex> case {:ok, "Hello World"} do
...>   {:ok, result} -> result
...>   {:error} -> "Uh oh!"
...>   _ -> "Catch all"
...> end
"Hello World"
```

The `_` variable is an important inclusion in `case/2` statements. Without it, failure to find a match will raise an error:

```elixir
iex> case :even do
...>   :odd -> "Odd"
...> end
** (CaseClauseError) no case clause matching: :even

iex> case :even do
...>   :odd -> "Odd"
...>   _ -> "Not Odd"
...> end
"Not Odd"
```

Consider `_` as the `else` that will match "everything else".

Since `case/2` relies on pattern matching, all of the same rules and restrictions apply.
If you intend to match against existing variables you must use the pin `^/1` operator:

```elixir
iex> pie = 3.14
 3.14
iex> case "cherry pie" do
...>   ^pie -> "Not so tasty"
...>   pie -> "I bet #{pie} is tasty"
...> end
"I bet cherry pie is tasty"
```

Another neat feature of `case/2` is its support for guard clauses:

_This example comes directly from the official Elixir [Getting Started](https://elixir-lang.org/getting-started/case-cond-and-if.html#case) guide._

```elixir
iex> case {1, 2, 3} do
...>   {1, x, 3} when x > 0 ->
...>     "Will match"
...>   _ ->
...>     "Won't match"
...> end
"Will match"
```

Check the official docs for [Expressions allowed in guard clauses](https://hexdocs.pm/elixir/guards.html#list-of-allowed-expressions).

## `cond`

When we need to match conditions rather than values we can turn to `cond/1`; this is akin to `else if` or `elsif` from other languages:

_This example comes directly from the official Elixir [Getting Started](https://elixir-lang.org/getting-started/case-cond-and-if.html#cond) guide._

```elixir
iex> cond do
...>   2 + 2 == 5 ->
...>     "This will not be true"
...>   2 * 2 == 3 ->
...>     "Nor this"
...>   1 + 1 == 2 ->
...>     "But this will"
...> end
"But this will"
```

Like `case/2`, `cond/1` will raise an error if there is no match.
To handle this, we can define a condition set to `true`:

```elixir
iex> cond do
...>   7 + 1 == 0 -> "Incorrect"
...>   true -> "Catch all"
...> end
"Catch all"
```

## `with`

The special form `with/1` is useful when you might use a nested `case/2` statement or situations that cannot cleanly be piped together. The `with/1` expression is composed of the keywords, the generators, and finally an expression.

We'll discuss generators more in the [list comprehensions lesson](../comprehensions/), but for now we only need to know they use [pattern matching](../pattern-matching/) to compare the right side of the `<-` to the left.

We'll start with a simple example of `with/1` and then look at something more:

```elixir
iex> user = %{first: "Sean", last: "Callan"}
%{first: "Sean", last: "Callan"}
iex> with {:ok, first} <- Map.fetch(user, :first),
...>      {:ok, last} <- Map.fetch(user, :last),
...>      do: last <> ", " <> first
"Callan, Sean"
```

In the event that an expression fails to match, the non-matching value will be returned:

```elixir
iex> user = %{first: "doomspork"}
%{first: "doomspork"}
iex> with {:ok, first} <- Map.fetch(user, :first),
...>      {:ok, last} <- Map.fetch(user, :last),
...>      do: last <> ", " <> first
:error
```

Now let's look at a larger example without `with/1` and then see how we can refactor it:

```elixir
case Repo.insert(changeset) do
  {:ok, user} ->
    case Guardian.encode_and_sign(user, :token, claims) do
      {:ok, token, full_claims} ->
        important_stuff(token, full_claims)

      error ->
        error
    end

  error ->
    error
end
```

When we introduce `with/1` we end up with code that is easy to understand and has fewer lines:

```elixir
with {:ok, user} <- Repo.insert(changeset),
     {:ok, token, full_claims} <- Guardian.encode_and_sign(user, :token, claims) do
  important_stuff(token, full_claims)
end
```


As of Elixir 1.3, `with/1` statements support `else`:

```elixir
import Integer

m = %{a: 1, c: 3}

a =
  with {:ok, number} <- Map.fetch(m, :a),
    true <- is_even(number) do
      IO.puts "#{number} divided by 2 is #{div(number, 2)}"
      :even
  else
    :error ->
      IO.puts("We don't have this item in map")
      :error

    _ ->
      IO.puts("It is odd")
      :odd
  end
```

It helps to handle errors by providing `case`-like pattern matching in it. The value passed is the first non-matched expression.

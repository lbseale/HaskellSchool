---
version: 1.0.0
title: Functions
---

Functions are at the heart of every Haskell program, and are first-class in the language.
You'll see some examples of functions below.

{% include toc.html %}

## Anonymous Functions

Anonymos functions, or Lambdas, are one of the building blocks of Haskell. They have the following syntax:

**Structure of a lambda**
```
 ╭─ "head", the declaration of the input bindigs
 │ 
 │ ╭─ Separator between head and body
 │ │ 
 │ │  ╭─ Body of the lambda
\x -> x
```

And incidentally, you have just seen one of the most primordial functions: The identity function, or `id`.  
This function is very simple, as it returns precisely the argument that you pass to it.

### Syntactic Sugars

#### Currying

In Haskell, all your functions have one argument. Inspired by the Lambda Calculus, this design is however extended in
the language through syntactic sugar that allows you to express yourself better:

```haskell
-- Syntactic sugar
\x y -> x + y
 
-- Currified version
\x -> \y -> x + y
```

The term is currification: Your function of two arguments has been transformed behind the scenes into a function
of one argument containing another function of one argument, containing itself the operation.  
In Javascript, this would be expressed like this:

```javascript
// Syntactic sugar
function (x, y) {
  return x + y;
}

// Currified version
function (x) {
  return function (y) {
    return x + y;
  };
}

> let a = function (x) { return function (y) { return x + y }; }
> a(1)
[Function (anonymous)]
> a(1)(2)
3
```

#### Tacit, or "point-free" style

Tacit style allows you to omit the arguments (or "points") of a function when declaring or applying it.  
While it can be quicker and help with equational reasoning, the loss of readability that it incurs needs to be a factor
in your decision to use it.

For example, the function that adds 1 to a number, `(\x -> x + 1)` can be reduced to `(+1)`.  
These functions are equivalent in every point except their syntax.

### Function Application

The function application syntax is the following:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> (\x y -> x + y) 2 3 
5
```

Here, the function between parentheses has been fully applied. But remember that these
are actually two functions on top of each-other, which means that,
much like in the above JavaScript example, it will return a function if you only pass it one argument:

```
(\x y -> x + y) 2
│ 
╰─ (\y -> 2 + y) 5
   │ 
   ╰─ 2 + 5
```

This ability to do partial application unlocks many abilities, one of which allowing you to build computations whose
parameters are not available all at the same time during the program's lifetime.

## Named functions

However, one can only go so far with anonymous functions. This is why you can give names to your functions:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> addOne x = x + 1
ghci> addOne 2
3
```

## Higher-order functions

There exists a third kind of functions, whose first argument is itself an anonymous functions: higher-order functions.
Because functions are first-class components of Haskell, they are no more magical than anonymous or named functions.

They become particularly useful when you need to control the application of a function over a parameter:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> applyTwice f x = f (f x)
ghci> applyTwice addOne 2
4
```

Some of the most well-known higher-order functions out there are `map` and `filter`

```console?lang=haskell&prompt=ghci>,ghci|
ghci> map (\x -> x + 2) [1,2,3,4]
[3,4,5,6]

ghci> filter (\x -> x > 3) [1,2,3,4,5,6]
[4,5,6]
```

## Local definitions: `where` & `let … in`

You can introduce bindings, such as local definition or intermediate results, through two different syntaxes:  
The `where` keyword, and the `let … in` construct.

### `where`

Here is an example of a function using `where` to store local functions:

```haskell
processInt x = timesTwo (addOne x)
  where
    addOne y = y + 1
    timesTwo z = z * 2
```

Other functions will be unable to use `addOne` and `timesTwo` since they belong to the scope of `processInt`.

### `let … in`

you can use its sibling, `let`, to produce the same result:

```haskell
processInt2 x =
  let addOne y = y + 1
      timesTwo z = z * 2  
   in timesTwo (addOne x)
```

These two syntaxes exist to better let you write down your mental process. Even though it is good form to stick with one
when in a collaborative setting (at work, for example), you are absolutely free to pick the one that makes you happy to
write Haskell. :)

## Function Composition

Function composition is one of the most primordial aspects of functional programming, and Haskell offers builtin operators
to enable it.

The `(.)` operator that is used for function composition is a legacy of the mathematical notation:

Let two functions  
```
f : X |-> Y  
g : Y |-> Z  
```

To go from X to Z, these two functions are composed like this:

`g(f(x))` -- read "g of f"

To simplify the notation, this composition is also written:

`g ∘ f : X |-> Z`

Which can be applied to an argument like this:

`(g ∘ f)(x)`

And this notation is so convenient that Haskell has kept it.

As such, if you have two or more functions that can fit like Lego bricks, nothing more is needed than simply getting
the arguments right. Remember the `processInt` function from earlier? It can now be written like this:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> let addOne y = y + 1
ghci> let timesTwo z = z * 2
ghci> let processInt3 x = (addOne . timesTwo) x
ghci> processInt3 2
5

-- And you can have some fun
ghci> let processInt4 x = (addOne . timesTwo . addOne . timesTwo) x
ghci> processInt4 5
23
```

---

And that is it for the basics about functions! In the next lesson, you will see that functions not only have a body, but also a type.  
You will see how to leverage types and make them a tool rather than a constraint.

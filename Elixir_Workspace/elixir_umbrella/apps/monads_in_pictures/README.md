# MonadsInPictures

Example of Monads in Elixir.

Uses the following libraries:
- [Witchcraft](https://github.com/expede/witchcraft)
- [Algae](https://github.com/expede/algae)

## Intro

### Define Maybe

```elixir
@type just(a) :: %Algae.Maybe.Just{just: a}
@type nothing() :: %Algae.Maybe.Nothing{}

@type maybe(a) :: just(a) | nothing()
```

### Operator Summary

Function Name | Operator         | Description
-------------:|------------------|-------------
`apply/2` | <code>&#124;></code> | `x -> (x -> y) -> y`
`map/2` | `~>`                   | `fx -> (x -> y) -> fy`
`ap/2` | `~>>`                   | `ax -> a(x -> y) -> ay`
`chain/2` | `>>>`                | `mx -> (x -> my) -> my`


## Functors

### Map (or fmap) operator: `~>`

```elixir
use Witchcraft
alias Algae.Maybe

Maybe.new(2) # %Algae.Maybe.Just{just: 2}
Maybe.new() # %Algae.Maybe.Nothing{}
plus3 = fn x -> x+3 end

# fmap :: fa -> (a -> b) -> fb
lift(value, plus3) # %Algae.Maybe.Just{just: 5}

# '<$>' operator in Haskell (infix form of fmap)
Maybe.new(2) ~> plus3 # %Algae.Maybe.Just{just: 5}
Maybe.new()  ~> plus3 # %Algae.Maybe.Nothing{}
```

### List Example
```elixir
use Witchcraft

# Map over values
[2, 4, 6] ~> plus3 # [5, 7, 9]
```

## Applicatives

### Ap operator: `~>>`

```elixir
use Witchcraft
alias Algae.Maybe

plus3 = fn x -> x+3 end

# '<*>' operator in Haskell
Maybe.new(2) ~>> Maybe.new(plus3) # %Algae.Maybe.Just{just: 5}
Maybe.new()  ~>> Maybe.new(plus3) # %Algae.Maybe.Nothing{}
```

### List Example

```elixir
use Witchcraft

plus3  = fn x -> x+3 end
double = fn y -> y*2 end

[double, plus3] <<~ [1,2,3]
# [double(1), double(2), double(3), plus3(1), plus3(2), plus3(3)]
# [2, 4, 6, 4, 5, 6]

[1,2,3] ~>> [double, plus3]
# [double(1), plus3(1), double(2), plus3(2), double(3), plus3(3)]
# [2, 4, 4, 5, 6, 6]
```

## Monads

### Chain (or bind) operator: `>>>`

```elixir
use Witchcraft
alias Algae.Maybe

import Integer, only: [is_even: 1]

half = fn
  x when is_even(x) -> x |> div(2) |> Maybe.new()
  x -> Maybe.new()
end

# bind :: ma -> (a -> mb) -> mb
Maybe.new(3) >>> half # %Algae.Maybe.Nothing{}
Maybe.new(4) >>> half # %Algae.Maybe.Just{just: 2}
Maybe.new()  >>> half # %Algae.Maybe.Nothing{}

Maybe.new(20) >>> half                    # %Algae.Maybe.Just{just: 10}
Maybe.new(20) >>> half >>> half           # %Algae.Maybe.Just{just: 5}
Maybe.new(20) >>> half >>> half >>> half  # %Algae.Maybe.Nothing{}
```

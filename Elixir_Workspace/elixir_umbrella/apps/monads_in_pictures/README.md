# MonadsInPictures

Example of Monads in Elixir.

### Intro

Define Maybe:
```elixir
@type just(a) :: %Algae.Maybe.Just{just: a}
@type nothing() :: %Algae.Maybe.Nothing{}

@type maybe(a) :: just(a) | nothing()
```

### Functors

```elixir
use Witchcraft
alias Algae.Maybe

Maybe.new(2) # %Algae.Maybe.Just{just: 2}
Maybe.new() # %Algae.Maybe.Nothing{}
plus3 = fn x -> x+3 end

# fmap
lift(value, plus3) # %Algae.Maybe.Just{just: 5}

# infix '<$>' operator
Maybe.new(2) ~> plus3 # %Algae.Maybe.Just{just: 5}
Maybe.new()  ~> plus3 # %Algae.Maybe.Nothing{}
```

### Applicatives

```elixir
use Witchcraft
alias Algae.Maybe

plus3 = fn x -> x+3 end

# infix '<*>' operator
Maybe.new(2) ~>> Maybe.new(plus3) # %Algae.Maybe.Just{just: 5}
Maybe.new()  ~>> Maybe.new(plus3) # %Algae.Maybe.Nothing{}
```

### Monads

```elixir
use Witchcraft
alias Algae.Maybe

import Integer, only: [is_even: 1]

half = fn
  x when is_even(x) -> x |> div(2) |> Maybe.new()
  x -> Maybe.new()
end

Maybe.new(3) >>> half # %Algae.Maybe.Nothing{}
Maybe.new(4) >>> half # %Algae.Maybe.Just{just: 2}
Maybe.new()  >>> half # %Algae.Maybe.Nothing{}

Maybe.new(20) # %Algae.Maybe.Just{just: 20}
>>> half      # %Algae.Maybe.Just{just: 10}
>>> half      # %Algae.Maybe.Just{just: 5}
>>> half      # %Algae.Maybe.Nothing{}
```

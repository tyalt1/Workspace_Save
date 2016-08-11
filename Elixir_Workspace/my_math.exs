#!/usr/bin/env elixir
# Example of several Elixir features
defmodule MyMath do
  require Integer #Integer module is loaded into VM, in not already
  alias :math, as: Math #Rename Erlang module
  import Stream #Merge Stream module into scope (use iterate, map, and filter)

  defmodule Basic do #Nested modules!
    def inc(n), do: n+1
    def dec(n), do: n-1
  end

  def is_prime(2), do: true
  def is_prime(n) when Integer.is_even(n), do: false
  def is_prime(n) when n > 1, do: prime(n, Math.sqrt(n), 3)
  defp prime(n, root, i) do
    cond do
      i > root -> true
      rem(n, i) === 0 -> false
      true -> prime(n, root, i+2)
    end
  end

  def fact(n), do: fact(n,1)
  defp fact(0, acc), do: acc
  defp fact(n, acc), do: fact(n-1,acc*n)

  def prime_stream, do: iterate(2, &Basic.inc/1) |> filter(&is_prime/1)

  def fib_stream(f1 \\ 0, f2 \\ 1) do # default values, compiles to be multiple function clauses
    iterate({f1, f2}, fn {x,y} -> {y,x+y} end) |> map(fn {x,_} -> x end)
  end

  def factors(n) do
    import List, only: [flatten: 1]
    for x <- 1..round(Math.sqrt(n)), rem(n, x) === 0 do
      [x, div(n,x)]
    end
    |> flatten
    |> Enum.uniq
    |> Enum.sort
  end

  def divisors(n), do: factors(n) -- [n]
end

MyMath.Basic.inc(1) |> IO.inspect
MyMath.prime_stream |> Enum.take(10) |> IO.inspect
MyMath.factors(20) |> IO.inspect
MyMath.divisors(20) |> IO.inspect

# Example of how to use protocols

defprotocol Blank do
  # @fall_back_any true #any type without implmentation will default to Any
  def is_blank(data)
end

defimpl Blank, for: List do
  def is_blank([]), do: true
  def is_blank(_), do: false
end

defimpl Blank, for: Tuple do
  def is_blank({}), do: true
  def is_blank(_), do: false
end

defimpl Blank, for: Map do
  def is_blank(m), do: map_size(m) == 0
end

defimpl Blank, for: Any do
  def is_blank(_), do: false
end

defmodule User do
  # @derive Blank #use Any impl for Blank

  #implment your own
  defimpl Blank, for: __MODULE__ do
    def is_blank(_), do: false
  end

  defstruct [name: "john", password: "****"]
end

# Method for implmentation of protocol for user defined type:
# 1. fall_back_any set to true
# 2. derive protocol
# 3. implment
# note choosing more then one may lead to warnings and logic errors

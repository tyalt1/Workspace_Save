# Example of how to use protocols

defprotocol Blank do
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
  @derive Blank #use Any impl for Blank

  #or implment your own
  defimpl Blank, for: __MODULE__ do
    def is_blank(_), do: false
  end

  defstruct [name: "john", password: "****"]
end

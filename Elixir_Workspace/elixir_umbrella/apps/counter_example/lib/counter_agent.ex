defmodule Counter.Agent do

  #Public API
  def start_link(n \\ 0) do
    Agent.start_link(fn-> n end)
  end

  def inc(pid) do
    Agent.update(pid, fn(n) -> n+1 end)
  end

  def dec(pid) do
    Agent.update(pid, fn(n) -> n-1 end)
  end

  def get(pid) do
    Agent.get(pid, fn(n) -> n end)
  end
end

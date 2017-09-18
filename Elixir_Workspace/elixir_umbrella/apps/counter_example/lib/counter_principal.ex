defmodule Counter.Principal do

  #Public API
  def start_link(n \\ 0) do
    pid = spawn_link(fn-> loop(n) end)
    {:ok, pid}
  end

  def inc(pid) do
    send(pid, :inc)
    :ok
  end

  def dec(pid) do
    send(pid, :dec)
    :ok
  end

  def get(pid) do
    send(pid, {:get, self()})
    receive do
      n when is_integer(n) -> {:ok, n}
    end
  end

  #Private
  defp loop(n) do
    receive do
      :inc -> n+1
      :dec -> n-1
      {:get, pid} -> send(pid, n)
    end
    |> loop()
  end
end

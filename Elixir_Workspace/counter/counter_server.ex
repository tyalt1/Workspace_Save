defmodule Counter.Server do
  use GenServer

  #Public API
  def start_link(n \\ 0) do
    GenServer.start_link(__MODULE__, n)
  end

  def inc(pid) do
    GenServer.cast(pid, :inc)
  end

  def dec(pid) do
    GenServer.cast(pid, :dec)
  end

  def get(pid) do
    GenServer.call(pid, :get)
  end

  #Callbacks
  def handle_cast(:inc, n) do
    {:noreply, n+1} #{:noreply, new_state}
  end
  def handle_cast(:dec, n) do
    {:noreply, n-1} #{:noreply, new_state}
  end
  
  def handle_call(:get, _from, n) do
    {:reply, {:ok, n}, n} #{:reply, return, new_state}
  end
end

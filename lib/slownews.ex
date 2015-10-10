defmodule Slownews do
  use Application
  require Logger

  def start(_type, _args) do
    opts = []
    port = Application.get_env(:slownews, :port)
    opts = Keyword.put(opts, :port, String.to_integer(port))

    Slownews.Supervisor.start_link(opts)
  end
end

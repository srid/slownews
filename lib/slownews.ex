defmodule Slownews do
  use Application
  require Logger

  def start(_type, _args) do
    opts = []
    port = Application.get_env(:slownews, :port)

    # Cowboy options, passed down by the supervisor:
    opts = opts
      |> Keyword.put(:port, String.to_integer(port))

    Slownews.Supervisor.start_link(opts)
  end
end

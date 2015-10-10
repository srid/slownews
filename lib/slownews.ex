defmodule Slownews do
  use Application

  def start(_type, _args) do
    opts = []
    port = Application.get_env(:slownews, :port)
    opts = Keyword.put(opts, :port, String.to_integer(port))

    Slownews.Supervisor.start_link(opts)
  end

  def fetchAll() do
    sites()
    |> Enum.map(&makeSite/1)
    |> Enum.map(&Slownews.Site.fetch/1)
  end

  def sites() do
    Application.get_env(:slownews, :sites)
    |> String.split(":")
  end

  def makeSite(site) do
    Slownews.Site.Reddit.new site
  end
end

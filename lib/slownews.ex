defmodule Slownews do
  use Application
  require Logger

  def start(_type, _args) do
    opts = []
    port = Application.get_env(:slownews, :port)
    opts = Keyword.put(opts, :port, String.to_integer(port))

    Logger.info "Pulling initial data"
    fetchAll()

    Slownews.Supervisor.start_link(opts)
  end

  def fetchAll() do
    :ets.new(:site_results, [:named_table, :set, :protected])
    sites()
    |> Enum.map(&makeSite/1)
    |> Enum.map(&fetchAndStore/1)
  end

  def fetchAndStore(site) do
    results = Slownews.Site.fetch(site)
    :ets.insert(:site_results, {to_string(site), results})
  end

  def getSiteResults() do
    :ets.tab2list(:site_results)
    |> Enum.into(Map.new)
  end

  def sites() do
    Application.get_env(:slownews, :sites)
    |> String.split(":")
  end

  def makeSite(site) do
    Slownews.Site.Reddit.new site
  end

end

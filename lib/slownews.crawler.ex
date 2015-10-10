defmodule Slownews.Crawler do
  use GenServer
  require Logger
  alias Slownews.Crawler.Util, as: Util

  @singleton_name :slownews_crawler

  def fetch(siteName) do
    GenServer.call(@singleton_name, {:fetch, siteName})
  end

  def fetchAll() do
    GenServer.call(@singleton_name, :fetchAll)
  end

  def start_link(state, opts) do
    Util.initializeTable
    _fetchAll()
    GenServer.start_link(__MODULE__, state, opts)
  end

  def handle_cast({:fetch, siteName}, _state) do
    siteName
    |> Util.makeSite
    |> Util.fetchAndStore
  end

  def handle_cast(:fetchAll, state) do
    _fetchAll()
  end

  def _fetchAll() do
    Logger.info "Fetching all sites"
    Util.sites()
    |> Enum.map(&Util.makeSite/1)
    |> Enum.map(&Util.fetchAndStore/1)
  end

  def child_spec() do
    import Supervisor.Spec
    worker(Slownews.Crawler, [[], []], [name: @singleton_name])
  end
end

defmodule Slownews.Crawler.Util do
  def initializeTable() do
    :ets.new(:site_results, [:named_table, :set, :protected])
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

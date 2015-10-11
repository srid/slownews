require Logger

defmodule Slownews.Crawler do
  use GenServer
  alias Slownews.Crawler.Util, as: Util

  @singleton_name :slownews_crawler

  def fetch(siteName) do
    GenServer.call(@singleton_name, {:fetch, siteName})
  end

  def fetchAll() do
    GenServer.cast(@singleton_name, :fetchAll)
  end

  def child_spec() do
    import Supervisor.Spec
    worker(Slownews.Crawler, [[], [name: @singleton_name]])
  end

  def start_link(state, opts) do
    GenServer.start_link(__MODULE__, state, opts)
  end

  def init(state) do
    Util.initializeTable
    _fetchAll("bootstrapping")
    {:ok, state}
  end

  def handle_cast({:fetch, siteName}, _state) do
    siteName
    |> Util.makeSite
    |> Util.fetchAndStore
  end

  def handle_cast(:fetchAll, state) do
    _fetchAll("scheduled")
    {:noreply, state}
  end

  def _fetchAll(reason) do
    Logger.info "Fetching all sites (#{reason})"
    Util.sites()
    |> Enum.map(&Util.makeSite/1)
    |> Enum.map(&Util.fetchAndStore/1)
    Logger.info "Done fetching all sites (#{reason})"
  end

end

defmodule Slownews.Crawler.Util do
  def initializeTable() do
    :ets.new(:site_results, [:named_table, :set, :protected])
  end

  def fetchAndStore(site) do
    Logger.info "Fetching site #{site}"
    results = Slownews.Site.fetch(site)
    :ets.insert(:site_results, {to_string(site), results})
    Logger.info "Stored reddit #{site} in ETS"
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
    case site do
      "hn" ->
        Slownews.Site.HackerNews.new
      _ ->
        Slownews.Site.Reddit.new site
    end
  end
end

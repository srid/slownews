require Logger

defmodule Slownews.Site.HNSearch do
  defstruct query: "erlang", max: 5

  def new(query, opts \\ []) do
    max = Keyword.get(opts, :max, 5)
    %Slownews.Site.HNSearch{query: query, max: max}
  end

  defimpl Slownews.Site, for: Slownews.Site.HNSearch do
    def fetch(site) do
      opts = Application.get_env(:slownews, :hackney_opts)
      Slownews.Site.HNSearch.Client.get!(site, [], opts).body
      |> Enum.take(site.max)
    end
  end

  defimpl String.Chars, for: Slownews.Site.HNSearch do
    def to_string(site) do
      Slownews.Site.Spec.new("hn/#{site.query}", max: site.max)
    end
  end
end

defmodule Slownews.Site.HNSearch.Client do
  use HTTPoison.Base

  @min_comments 3

  def process_url(siteSpec) do
    site = Slownews.Site.Factory.newFromSpec(siteSpec)
    now =  :os.system_time(:seconds)
    one_week_ago = now - 604800
    "http://hn.algolia.com/api/v1/search?tags=story&numericFilters=created_at_i>#{one_week_ago}&hitsPerPage=#{site.max}&query=#{site.query}"
  end

  def process_response_body(data) do
    data
    |> Poison.decode!
    |> get_in(["hits"])
    |> Enum.filter(&select_link/1)
    |> Enum.map(&transform_link/1)
  end

  def select_link(link) do
    link["num_comments"] > @min_comments
  end

  def transform_link(link) do
    meta_url = "https://news.ycombinator.com/item?id=" <> to_string(link["objectID"])
    %{title: link["title"],
      url: link["url"],
      meta_url: meta_url,
      created: link["created_at_i"]}
  end
end

require Logger

defmodule Slownews.Site.HackerNews do
  defstruct max: 20

  def new(_name, opts \\ []) do
    max = Keyword.get(opts, :max, 7)
    %Slownews.Site.HackerNews{max: max}
  end 

  defimpl Slownews.Site, for: Slownews.Site.HackerNews do
    def fetch(site) do
      Slownews.Site.HackerNews.Client.getBest!(site.max)
    end
  end

  defimpl String.Chars, for: Slownews.Site.HackerNews do
    def to_string(hn) do
      Slownews.Site.Spec.new("hn", max: hn.max)
    end
  end
end

defmodule Slownews.Site.HackerNews.Client do
  use HTTPoison.Base

  def getBest!(maxlinks) do
    # TODO: parallelize fetches
    # TODO: cache fetching of individual links? (only matters during development)
    opts = Application.get_env(:slownews, :hackney_opts)
    get!("best", opts).body
    |> Enum.take(maxlinks)
    |> Enum.map(&get!(&1, opts).body)
    |> Enum.map(&transform_link/1)
  end

  def process_url("best") do
    "https://hacker-news.firebaseio.com/v0/beststories.json"
  end

  def process_url(link_id_string) do
    link_id = String.to_integer(link_id_string)
    "https://hacker-news.firebaseio.com/v0/item/#{link_id}/.json"
  end

  def process_response_body(data) do
    data
    |> Poison.decode!
  end

  def transform_link(link) do
    meta_url = "https://news.ycombinator.com/item?id=" <> to_string(link["id"])
    %{title: link["title"],
      url: link["url"] || meta_url,
      meta_url: meta_url,
      created: link["time"]}
  end
end

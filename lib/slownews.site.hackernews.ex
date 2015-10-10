require Logger

defmodule Slownews.Site.HackerNews do
  defstruct dummy: __MODULE__

  def new(), do: %Slownews.Site.HackerNews{}

  defimpl Slownews.Site, for: Slownews.Site.HackerNews do
    def fetch(_site) do
      Slownews.Site.HackerNews.Client.getBest!()
    end
  end

  defimpl String.Chars, for: __MODULE__ do
    def to_string(hnSite), do: "hn"
  end
end

defmodule Slownews.Site.HackerNews.Client do
  use HTTPoison.Base

  @maxlinks 2

  def getBest! do
    # TODO: parallelize fetches
    # TODO: cache fetching of individual links? (only matters during development)
    Logger.info "Fetching HackerNews"
    get!("best").body
    |> Enum.take(@maxlinks)
    |> Enum.map(&get!(&1).body)
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
    %{title: link["title"],
      url: link["url"],
      meta_url: "https://news.ycombinator.com?id=" <> to_string(link["id"]),
      created: link["time"]}
  end
end

require Logger

defmodule Slownews.Site.Reddit do
  defstruct subreddit: "reddit.com"

  def new(name), do: %Slownews.Site.Reddit{subreddit: name}

  defimpl Slownews.Site, for: Slownews.Site.Reddit do
    def fetch(redditSite) do
      Logger.info "Fetching reddit #{redditSite.subreddit}"
      Slownews.Site.Reddit.Client.get!(redditSite.subreddit, [], [recv_timeout: 60*1000]).body
    end
  end

  defimpl String.Chars, for: Slownews.Site.Reddit do
    def to_string(redditSite), do: redditSite.subreddit
  end
end

defmodule Slownews.Site.Reddit.Client do
  use HTTPoison.Base

  def process_url(subreddit) do
    "https://www.reddit.com/r/#{subreddit}/top/.json?sort=top&t=week"
  end

  def process_response_body(data) do
    data
    |> Poison.decode!
    |> get_in(["data", "children"])
    |> Enum.map(&(get_in(&1, ["data"]) |> transform_link))
  end

  def transform_link(link) do
    %{title: link["title"],
      url: link["url"],
      meta_url: "https://www.reddit.com" <> link["permalink"],
      created: link["created_utc"]}
  end
end

defmodule Slownews.Site.Reddit do
  use HTTPoison.Base
  defstruct subreddit: "reddit.com"

  @expected_fields ~w(title permalink created_utc)

  def new(name), do: %Slownews.Site.Reddit{subreddit: name}

  def process_url(subreddit) do
    "https://www.reddit.com/r/#{subreddit}.json"
  end

  def process_response_body(data) do
    data
    |> Poison.decode!
    |> get_in(["data", "children"])
    |> Enum.map(&(get_in(&1, ["data"]) |> Dict.take(@expected_fields)))
  end

  defimpl Slownews.Site, for: Slownews.Site.Reddit do
    def fetch(redditSite) do
      Slownews.Site.Reddit.get!(redditSite.subreddit)
    end
  end
end

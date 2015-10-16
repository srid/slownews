require Logger

defmodule Slownews.Site.Reddit do
  defstruct subreddit: "r/reddit.com", max: 20

  def new(name, opts \\ []) do
    max = Keyword.get(opts, :max, 20)
    %Slownews.Site.Reddit{subreddit: name, max: max}
  end

  defimpl Slownews.Site, for: Slownews.Site.Reddit do
    def fetch(redditSite) do
      opts = Application.get_env(:slownews, :hackney_opts)
      Slownews.Site.Reddit.Client.get!(redditSite, [], opts).body
      |> Enum.take(redditSite.max)
    end
  end

  defimpl String.Chars, for: Slownews.Site.Reddit do
    def to_string(redditSite) do
      Slownews.Site.Spec.new(redditSite.subreddit, max: redditSite.max)
    end
  end
end

defmodule Slownews.Site.Reddit.Client do
  use HTTPoison.Base

  def process_url(siteSpec) do
    site = Slownews.Site.Factory.newFromSpec(siteSpec)
    "https://www.reddit.com/#{site.subreddit}/top/.json?sort=top&t=week&limit=#{site.max}"
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

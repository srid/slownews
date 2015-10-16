defprotocol Slownews.Site do
  @doc "Fetch content as list of links with :title, :url, :meta_url"
  def fetch(site)
end

defmodule Slownews.Site.Spec do
  @re Regex.compile!("(?<name>[^#]+)(#(?<opts>.*))?")

  def new(name, opts \\ []) do
    optsSpec = opts |> stringifyKeywordList |> Enum.join(":")
    case optsSpec do
      "" -> name
      _  -> Enum.join([name, optsSpec], "#")
    end
  end

  def parse(spec) do
    %{"name" => name, "opts" => opts} = Regex.named_captures(@re, spec)
    case opts do
      "" -> {name, []}
      _  -> {name, String.split(opts, ",")
                     |> Enum.map(&String.split(&1, "=", parts: 2))
                     |> Enum.map(&parseOpt/1)}
    end
  end

  defp parseOpt(pair) when is_list(pair) do
    [key, value] = pair
    case key do
      "max" -> case Integer.parse(value) do
                 {num, ""} -> {:max, num}
               end
    end
  end

  defp stringifyKeywordList(kl) do
    kl |> Enum.map(fn ({k,v}) -> to_string(k) <> "=" <> to_string(v) end)
  end
end

defmodule Slownews.Site.Factory do
  def newFromSpec(spec) do
    spec |> Slownews.Site.Spec.parse |> makeSite
  end

  defp makeSite({name, opts}) do
    case name do
      "hn" ->
        Slownews.Site.HackerNews.new opts
      _ ->
        Slownews.Site.Reddit.new name, opts
    end
  end
end

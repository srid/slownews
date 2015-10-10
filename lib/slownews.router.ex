defmodule Slownews.Router do
  use Plug.Router

  if Mix.env == :dev do
    use Plug.Debugger
  end

  use Plug.ErrorHandler

  plug Plug.Logger
  plug Plug.Static, at: "/", from: "web/static"

  plug :match
  plug :dispatch

  get "/" do
    conn
    |> put_resp_header("location", "/index.html")
    |> send_resp(conn.status || 302, "Moved")
  end

  get "/data" do
    siteNames = Slownews.Crawler.Util.sites
    allSites = Slownews.Crawler.Util.getSiteResults
    data = siteNames
    |> Enum.map(fn (siteName) -> %{name: siteName, links: Map.get(allSites, siteName)} end)
    |> Poison.encode!
    send_resp(conn, 200, data)
  end

  match _ do
    send_resp(conn, 404, "404, may I help you?")
  end

  defp handle_errors(conn, %{kind: _kind, reason: _reason, stack: _stack}) do
    send_resp(conn, conn.status, "Something went wrong")
  end
end

defmodule Slownews.Router do
  use Plug.Router

  if Mix.env == :dev do
    use Plug.Debugger
  end

  use Plug.ErrorHandler

  plug Plug.Logger
  plug Plug.Static, at: "/", from: "web/static"

  plug CORSPlug, origin: ["*"]

  plug :match
  plug :dispatch

  get "/" do
    conn
    |> put_resp_header("content-type", "text/html")
    |> send_file(200, "web/static/index.html")
  end

  get "/data" do
    siteKeys = Slownews.Crawler.Util.sites |> Enum.map(&to_string/1)
    data = Slownews.Crawler.Util.getLinksFor(siteKeys)
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

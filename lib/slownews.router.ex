defmodule Slownews.Router do
  use Trot.Router

  plug Plug.Logger
  plug Plug.Static, at: "/", from: "web/static"

  get "/" do
    {:redirect, "/index.html"}
  end

  use Trot.NotFound
end

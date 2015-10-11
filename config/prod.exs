use Mix.Config

config :quantum, cron: [
  "0 * * * *": &Slownews.Crawler.fetchAll/0
]

use Mix.Config

config :quantum, cron: [
  "*/15 * * * *": &Slownews.Crawler.fetchAll/0
]

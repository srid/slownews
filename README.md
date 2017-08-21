# 🐢 slownews

A web app that aggregates best news during last week from sites like reddit and Hacker News. Uses Elixir for backend and Miso for frontend. 

Backend is written in **Elixir**; frontend in **Haskell**.

## Running locally

Needs Elixir 1.4+:

```
cd backend && mix deps.get
SITES="hn/github:hn/india:hn#max=5:r/programming#max=5:r/videos#max=2" make
```

Visit http://localhost:3000/

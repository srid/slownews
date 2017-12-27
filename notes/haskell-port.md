# Elixir to Haskell port

In December 2017 I ported the [slownews](https://github.com/srid/slownews) backend from Elixir to Haskell because of my desire to use Haskell in writing non-toy projects. The frontend is already written in Haskell (using [Miso](https://haskell-miso.org)), therefore it was only natural that the backend be written in Haskell too.

## Why Haskell?

Haskell's modern type system makes it **safer and enjoyable** to make wide-ranging code modifications to a project without worrying too much about breaking something. This confidence is the main reason why I find Haskell so appealing. In contrast, in languages like Python and Go I would have to deal with runtime bugs after every non-trivial change; issues that are normally handled by Haskell's type system and the compiler.

Secondly, Haskell is a **pure** functional programming language, and everything is immutable by default. Mutations and side-effects are handled in pure code. This makes code easier to reason about, which only adds to that aforementioned confidence.

Finally, Haskell gives me a reason to push the boundaries of learning. I particularly enjoy learning something new, something that I'm passionate about. As far as life in general is concerned I do not like becoming disengaged or inactive. Life is meant to be lived curiously with an active enthusiasm.

## What did I learn?

I think SlowNews turned out to be a good first project (not counting my port of the frontend from [Elm to Haskell](https://github.com/srid/slownews/pull/10)) for me as it is just at the right level of compexity. I ended up using/ learning the following:

1. Using `aeson` ([tutorial](https://artyom.me/aeson)) to parse JSON in type-safe manner
1. Using `wreq` ([tutorial](http://www.serpentine.com/wreq/tutorial.html)), in conjunction with `aeson`, to fetch reddit and HN api 
1. Using `katip` for logging
1. Using `envy` for environment variable handling
1. Using `Either` and `Applicative` to deal with errors without complicating code
1. Using `stm` (Software Transactional Memory) for concurrency
1. Using stack's [docker integration](https://docs.haskellstack.org/en/stable/docker_integration/) for seamless Heroku deployment
1. [Spacemacs and intero](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/haskell) for Haskell development environment.

I'm grateful to the [FP Slack](https://fpchat-invite.herokuapp.com) and Slackoverflow communities for enabling me to ask questions and discuss about Haskell with folks from around the world.

## What's next?

I still need to learn and become familiar with advanced concepts (like monad transformers). In my experience learning happens through both reading and doing. So I just need to continue doing what I'm already doing. My goal is to become proficient in Haskell and find a way to use it both in my personal and professional time. 

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SlowNews.Stack where

import Katip (KatipContextT)

type Stack a = KatipContextT IO a

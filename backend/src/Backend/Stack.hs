{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Stack where

import Katip (KatipContextT)

type Stack a = KatipContextT IO a

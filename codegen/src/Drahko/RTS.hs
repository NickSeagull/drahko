{-# LANGUAGE TemplateHaskell #-}

module Drahko.RTS where

import Data.FileEmbed
import Relude
import System.FilePath

contents :: Text
contents = $(embedStringFile $ "codegen" </> "rts" </> "rts.ahk")

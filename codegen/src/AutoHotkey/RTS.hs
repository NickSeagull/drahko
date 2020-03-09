{-# LANGUAGE TemplateHaskell #-}

module AutoHotkey.RTS where

import Data.FileEmbed
import Relude
import System.FilePath

rts :: Text
rts = $(embedStringFile $ "codegen" </> "rts" </> "rts.ahk")

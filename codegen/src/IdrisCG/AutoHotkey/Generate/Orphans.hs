{-# OPTIONS_GHC -fno-warn-orphans #-}

module IdrisCG.AutoHotkey.Generate.Orphans where

import Data.Data
import qualified IRTS.Lang as Idris
import qualified Idris.Core.CaseTree as Idris

deriving instance Typeable Idris.FDesc

deriving instance Data Idris.FDesc

deriving instance Typeable Idris.LVar

deriving instance Data Idris.LVar

deriving instance Typeable Idris.PrimFn

deriving instance Data Idris.PrimFn

deriving instance Typeable Idris.CaseType

deriving instance Data Idris.CaseType

deriving instance Typeable Idris.LExp

deriving instance Data Idris.LExp

deriving instance Typeable Idris.LDecl

deriving instance Data Idris.LDecl

deriving instance Typeable Idris.LOpt

deriving instance Data Idris.LOpt

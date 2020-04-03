module Drahko.Generate.Common where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Drahko.Generate.Name
import Drahko.Syntax
import qualified Idris.Core.TT as Idris
import Relude

type UnusedNames = HashSet Name

nullExpr :: Expression
nullExpr = Literal (String "")

setUsed :: MonadState UnusedNames m => ToName a => a -> m ()
setUsed name =
  modify (HashSet.delete $ toName name)

toUnusedNames :: [Idris.Name] -> UnusedNames
toUnusedNames namesList =
  fmap toName namesList
    & HashSet.fromList

filterUnused :: [Statement] -> UnusedNames -> [Statement]
filterUnused stmts unused = do
  let notMember a set = not (HashSet.member a set)
  let isUnused = \case
        Function n _ _ -> n `notMember` unused
        _ -> False
  filter isUnused stmts

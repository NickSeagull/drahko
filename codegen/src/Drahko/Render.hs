module Drahko.Render where

import qualified Data.Text as Text
import qualified Drahko.Syntax as Syntax
import Relude hiding (empty)
import Text.PrettyPrint.Mainland

renderBinaryOperator :: Syntax.BinaryOperator -> Doc
renderBinaryOperator = \case
  Syntax.Add -> "+"
  Syntax.Subtract -> "-"
  Syntax.Multiply -> "*"
  Syntax.Divide -> "/"
  Syntax.Equal -> "=="
  Syntax.LessThan -> "<"
  Syntax.LessThanEqual -> "<="
  Syntax.GreaterThan -> ">"
  Syntax.GreaterThanEqual -> ">="
  Syntax.Concat -> "."

renderLiteral :: Syntax.Literal -> Doc
renderLiteral = \case
  Syntax.Integer i -> integer i
  Syntax.Floating d -> double d
  Syntax.String txt ->
    show txt
      & Text.replace "\\" "`"
      & Text.replace "`\"" "\"\""
      & strictText
  Syntax.List expressions -> do
    let renderedExpressions = fmap renderExpression expressions
    bracketList renderedExpressions

renderExpression :: Syntax.Expression -> Doc
renderExpression = \case
  Syntax.BinaryOperatorApply op lhs rhs ->
    enclose lparen rparen (renderExpression lhs)
      <+> renderBinaryOperator op
      <+> parens (renderExpression rhs)
  Syntax.Literal lit -> renderLiteral lit
  Syntax.Variable name -> renderName name
  Syntax.Apply expr args ->
    renderExpression expr
      <> parenList (fmap renderExpression args)
  Syntax.Projection expr projExpr -> renderExpression expr <> brackets (renderExpression projExpr)
  Syntax.DotAccess e1 e2 ->
    renderExpression e1 <> dot <> renderExpression e2

renderName :: Syntax.Name -> Doc
renderName (Syntax.Name name) = strictText name

renderBlock :: Syntax.Block -> Doc
renderBlock statements =
  stack (fmap renderStatement statements)

renderConditionalCase :: Doc -> Syntax.ConditionalCase -> Doc
renderConditionalCase caseWord (Syntax.ConditionalCase expr block) =
  stack
    [ caseWord <> parens (renderExpression expr),
      lbrace,
      indent 4 $ renderBlock block,
      rbrace
    ]

renderConditionalStatement :: Syntax.ConditionalStatement -> Doc
renderConditionalStatement (Syntax.ConditionalStatement ifCase elseIfCases maybeElseCase) =
  renderConditionalCase "if" ifCase
    </> stack (map (renderConditionalCase "else if") elseIfCases)
    </> maybe empty renderElse maybeElseCase
  where
    renderElse block =
      stack
        [ "else",
          lbrace,
          renderBlock block,
          rbrace
        ]

renderStatement :: Syntax.Statement -> Doc
renderStatement = \case
  Syntax.Return expression -> "return" <+> renderExpression expression
  Syntax.While expression block ->
    stack
      [ "while" <+> renderExpression expression,
        lbrace,
        renderBlock block,
        rbrace
      ]
  Syntax.Break -> "break"
  Syntax.Continue -> "continue"
  Syntax.Function name args block ->
    stack
      [ renderName name <> parenList (fmap renderName args),
        lbrace,
        -- indent 4 "global",
        indent 4 $ renderBlock block,
        rbrace
      ]
  Syntax.Call nameExpr params ->
    renderExpression nameExpr
      <> parenList (fmap renderExpression params)
  Syntax.Condition conditionalStatement ->
    renderConditionalStatement conditionalStatement
  Syntax.Assignment nameExpression expression ->
    renderExpression nameExpression <+> ":=" <+> renderExpression expression
  Syntax.Command name expression -> do
    let prependPercent x = "%" <+> x
    renderName name <> comma <+> commasep (fmap (prependPercent . renderExpression) expression)
  Syntax.Class className extendsName block ->
    stack
      [ "class" <+> renderName className <+> maybe "" (\n -> "extends" <+> renderName n) extendsName,
        lbrace,
        indent 4 $ renderBlock block,
        rbrace
      ]
  Syntax.RawExpression expr ->
    renderExpression expr
  Syntax.NoOp ->
    ""

renderProgram :: Syntax.Program -> Doc
renderProgram (Syntax.Program statements) =
  stack (fmap renderStatement statements)

renderToText :: Doc -> Text
renderToText = toText . pretty 9999

parenList :: [Doc] -> Doc
parenList = parens . commasep

bracketList :: [Doc] -> Doc
bracketList = brackets . commasep

quoted :: Doc -> Doc
quoted = enclose dquote dquote

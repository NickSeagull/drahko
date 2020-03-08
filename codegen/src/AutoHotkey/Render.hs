module AutoHotkey.Render where

import qualified AutoHotkey.Syntax as AST
import Relude hiding (empty)
import Text.PrettyPrint.Mainland

renderBinaryOperator :: AST.BinaryOperator -> Doc
renderBinaryOperator = \case
  AST.Add -> "+"
  AST.Subtract -> "-"
  AST.Multiply -> "*"
  AST.Divide -> "/"
  AST.Equal -> "=="
  AST.LessThan -> "<"
  AST.LessThanEqual -> "<="
  AST.GreaterThan -> ">"
  AST.GreaterThanEqual -> ">="
  AST.Concat -> "."

renderLiteral :: AST.Literal -> Doc
renderLiteral = \case
  AST.Integer i -> integer i
  AST.Floating d -> double d
  AST.String txt -> strictText txt
  AST.List expressions -> do
    let renderedExpressions = fmap renderExpression expressions
    parenList renderedExpressions

renderExpression :: AST.Expression -> Doc
renderExpression = \case
  AST.BinaryOperatorApply op lhs rhs ->
    enclose lparen rparen (renderExpression lhs)
      <+> renderBinaryOperator op
      <+> parens (renderExpression rhs)
  AST.Literal lit -> renderLiteral lit
  AST.Variable name -> renderName name
  AST.Apply expr args -> renderExpression expr <> parenList (fmap renderExpression args)

renderName :: AST.Name -> Doc
renderName (AST.Name name) = strictText name

renderBlock :: AST.Block -> Doc
renderBlock statements =
  stack (fmap renderStatement statements)

renderConditionalCase :: Doc -> AST.ConditionalCase -> Doc
renderConditionalCase caseWord (AST.ConditionalCase expr block) =
  stack
    [ caseWord <> parens (renderExpression expr),
      lbrace,
      renderBlock block,
      rbrace
    ]

renderConditionalStatement :: AST.ConditionalStatement -> Doc
renderConditionalStatement (AST.ConditionalStatement ifCase elseIfCases maybeElseCase) =
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

renderStatement :: AST.Statement -> Doc
renderStatement = \case
  AST.Let name expression -> renderName name <+> ":=" <+> renderExpression expression
  AST.Return expression -> "return" <+> renderExpression expression
  AST.While expression block ->
    stack
      [ "while" <+> renderExpression expression,
        lbrace,
        renderBlock block,
        rbrace
      ]
  AST.Break -> "break"
  AST.Continue -> "continue"
  AST.Function name args block ->
    stack
      [ renderName name <> parenList (fmap renderName args),
        lbrace,
        renderBlock block,
        rbrace
      ]
  AST.Call name params ->
    "Func"
      <> parens (renderName name)
      <> dot
      <> "Bind"
      <> parenList (fmap renderExpression params)
      <> dot
      <> "Call()"
  AST.Condition conditionalStatement ->
    renderConditionalStatement conditionalStatement
  AST.Assignment name expression ->
    renderName name <+> ":=" <+> renderExpression expression
  AST.SubroutineCall name expression ->
    renderName name <> comma <+> commasep (fmap renderExpression expression)

renderProgram :: AST.Program -> Doc
renderProgram (AST.Program statements) =
  stack (fmap renderStatement statements)

parenList :: [Doc] -> Doc
parenList = parens . commasep

bracketList :: [Doc] -> Doc
bracketList = brackets . commasep

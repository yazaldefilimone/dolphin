import {
  Node,
  ProgramKind,
  StatementKind,
  ExpressionKind,
  Statement,
  Program,
  Expression,
  ExpressionStatement,
  IntegerLiteral,
} from "ast";
import { BaseObject } from "evaluator/object";
import { Maybe } from "utils";
import { Integer } from "./object/integer";

export function Evaluator(node: Maybe<Node>): Maybe<BaseObject> {
  if (node === null) return null;
  switch (node.kind) {
    case ProgramKind.program:
      return evaluatorStatements((node as Program).statements);
    case StatementKind.EXPRESSION:
      return Evaluator((node as ExpressionStatement).expression);
    case ExpressionKind.INTEGER:
      return new Integer((node as IntegerLiteral).value);
    default:
      return null;
  }
}

function evaluatorStatements(statements: Statement[]): Maybe<BaseObject> {
  let result: Maybe<BaseObject> = null;
  for (const statement of statements) {
    result = Evaluator(statement);
  }
  return result;
}

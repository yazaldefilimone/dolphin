import {
  Node,
  ProgramKind,
  StatementKind,
  ExpressionKind,
  Statement,
  Program,
  ExpressionStatement,
  IntegerLiteral,
  BooleanLiteral,
} from "ast";
import {
  BaseObject,
  InternalBoolean,
  Integer,
  internal,
} from "evaluator/object";

import { Maybe } from "utils";

export function Evaluator(node: Maybe<Node>): Maybe<BaseObject> {
  if (node === null) return null;
  switch (node.kind) {
    case ProgramKind.program:
      return evaluatorStatements((node as Program).statements);
    case StatementKind.EXPRESSION:
      return Evaluator((node as ExpressionStatement).expression);
    case ExpressionKind.INTEGER:
      return new Integer((node as IntegerLiteral).value);
    case ExpressionKind.BOOLEAN:
      return nativeBoolToBooleanObject((node as BooleanLiteral).value);
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

function nativeBoolToBooleanObject(input: boolean): InternalBoolean {
  return input ? internal.TRUE : internal.FALSE;
}

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
  PrefixExpression,
} from "ast";
import { BaseObject, InternalBoolean, Integer, internal, objectType } from "evaluator/object";

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
    case ExpressionKind.PREFIX:
      const prefixNode = node as PrefixExpression;
      const right = Evaluator(prefixNode.right);
      if (right === null) return null;
      return evaluatorPrefixExpression(prefixNode.operator, right);
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
function evaluatorPrefixExpression(operator: string, right: BaseObject): Maybe<BaseObject> {
  switch (operator) {
    case "!":
      return evaluatorBangOperatorExpression(right);
    case "-":
      return evaluatorMinusOperatorExpression(right);
    default:
      return null;
  }
}

function evaluatorMinusOperatorExpression(right: BaseObject<any>): Maybe<BaseObject> {
  if (right.type() !== objectType.INTEGER) {
    return null;
  }
  const value = (right as unknown as IntegerLiteral).value;
  return new Integer(-value);
}

function evaluatorBangOperatorExpression(right: BaseObject): BaseObject {
  switch (right.value) {
    case internal.TRUE.value:
      return internal.FALSE;
    case internal.FALSE.value:
      return internal.TRUE;
    case internal.NULL.value:
      return internal.TRUE;
    default:
      return internal.FALSE;
  }
}
function nativeBoolToBooleanObject(input: boolean): InternalBoolean {
  return input ? internal.TRUE : internal.FALSE;
}

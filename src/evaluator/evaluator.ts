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
  InfixExpression,
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
      return nativeBooleanObject((node as BooleanLiteral).value);
    case ExpressionKind.PREFIX:
      const prefixNode = node as PrefixExpression;
      const prefixRight = Evaluator(prefixNode.right);
      if (prefixRight === null) return null;
      return evaluatorPrefixExpression(prefixNode.operator, prefixRight);
    case ExpressionKind.INFIX:
      const infix = node as InfixExpression;
      const infixLeft = Evaluator(infix.left);
      const infixRight = Evaluator(infix.right);
      return evalIntegerInfixExpression(infixLeft, infix.operator, infixRight);
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
      return evalBangOperatorExpression(right);
    case "-":
      return evalMinusOperatorExpression(right);
    default:
      return null;
  }
}

function evalMinusOperatorExpression(right: BaseObject<any>): Maybe<BaseObject> {
  if (right.type() !== objectType.INTEGER) {
    return null;
  }
  const value = (right as unknown as IntegerLiteral).value;
  return new Integer(-value);
}

function evalBangOperatorExpression(right: BaseObject): BaseObject {
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

function evalIntegerInfixExpression(left: Maybe<BaseObject>, op: string, right: Maybe<BaseObject>): Maybe<BaseObject> {
  if (!isObject(left) || !isObject(right)) {
    return null;
  }
  const l = left as BaseObject<number>;
  const r = right as BaseObject<number>;
  switch (op) {
    case "+":
      return new Integer(l.value + r.value);
    case "/":
      return new Integer(l.value / r.value);
    case "*":
      return new Integer(l.value * r.value);
    case "-":
      return new Integer(l.value - r.value);
    default:
      return evalBooleanInfixExpression(left, op, right);
  }
}

function evalBooleanInfixExpression(left: Maybe<BaseObject>, op: string, right: Maybe<BaseObject>): Maybe<BaseObject> {
  const l = left as InternalBoolean;
  const r = right as InternalBoolean;
  switch (op) {
    case "<":
      return nativeBooleanObject(l.value < r.value);
    case ">":
      return nativeBooleanObject(l.value > r.value);
    case "==":
      return nativeBooleanObject(l.value === r.value);
    case "!=":
      return nativeBooleanObject(l.value !== r.value);
    default:
      return null;
  }
}

function isExpectNode(node: Maybe<BaseObject>, expectType: objectType): boolean {
  if (!isObject(node)) return false;
  return node?.type() == expectType;
}
function isObject(node: Maybe<BaseObject>): boolean {
  return node !== null;
}
function nativeBooleanObject(input: boolean): InternalBoolean {
  return input ? internal.TRUE : internal.FALSE;
}

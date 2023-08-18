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
  IfExpression,
  ReturnStatement,
} from "ast";

import { BaseObject, InternalBoolean, Integer, internal, EBaseObject, ReturnObject } from "evaluator/object";

import { Maybe } from "utils";
import { parseTwoObjectToString, typeMismatchError, unknownIdentifierError, unknownOperatorError } from "./errors";
import { verifyPermission } from "./permissions";

export function Evaluator(node: Maybe<Node>): Maybe<BaseObject> {
  if (node === null) return internal.NULL;
  switch (node.kind) {
    case ProgramKind.program:
      return evalProgram(node as Program);
    case StatementKind.BLOCK:
      return evalProgram(node as Program);
    case StatementKind.EXPRESSION:
      return Evaluator((node as ExpressionStatement).expression);
    case ExpressionKind.INTEGER:
      return new Integer((node as IntegerLiteral).value);
    case ExpressionKind.BOOLEAN:
      return nativeBooleanObject((node as BooleanLiteral).value);
    case ExpressionKind.PREFIX:
      const prefixNode = node as PrefixExpression;
      const prefixRight = Evaluator(prefixNode.right);
      if (prefixRight === null) return internal.NULL;
      if (isExpectObject(prefixRight, EBaseObject.ERROR)) return prefixRight;
      return evalPrefixExpression(prefixNode.operator, prefixRight);
    case ExpressionKind.INFIX:
      const infix = node as InfixExpression;
      const infixLeft = Evaluator(infix.left);
      const infixRight = Evaluator(infix.right);
      return evalIntegerInfixExpression(infixLeft, infix.operator, infixRight);
    case ExpressionKind.IF:
      const ifNode = node as IfExpression;
      return evalIfExpression(ifNode);
    case StatementKind.BLOCK:
      const blockNode = node as Program;
      return evaluatorStatements(blockNode.statements);
    case StatementKind.RETURN:
      const rNode = node as ReturnStatement;
      const returnValue = Evaluator(rNode.returnValue);
      if (returnValue === null) return internal.NULL;
      return new ReturnObject(returnValue);
    default:
      return unknownIdentifierError(node.kind);
  }
}

function evaluatorStatements(statements: Statement[]): Maybe<BaseObject> {
  let result: Maybe<BaseObject> = null;
  for (const statement of statements) {
    result = Evaluator(statement);
  }
  if (result === null) return internal.NULL;
  return result;
}
function evalPrefixExpression(operator: string, right: BaseObject): Maybe<BaseObject> {
  switch (operator) {
    case "!":
      return evalBangOperatorExpression(right);
    case "-":
      return evalMinusOperatorExpression(right);
    default:
      return unknownOperatorError(`${operator}${right.type()}`);
  }
}

function evalMinusOperatorExpression(right: BaseObject<any>): Maybe<BaseObject> {
  if (!isExpectObject(right, EBaseObject.INTEGER)) {
    return unknownOperatorError(`-${right.type()}`);
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
  const leftType = left ?? internal.NULL;
  const rightType = right ?? internal.NULL;
  if (!isEqual(left, right)) {
    return typeMismatchError(leftType, rightType, op);
  }
  if (!verifyPermission(op, leftType.type()) && !verifyPermission(op, rightType.type())) {
    return unknownOperatorError(parseTwoObjectToString(leftType.type(), rightType.type(), op));
  }
  const leftInteger = left as BaseObject<number>;
  const rightInteger = right as BaseObject<number>;
  switch (op) {
    case "+":
      return new Integer(leftInteger.value + rightInteger.value);
    case "/":
      return new Integer(leftInteger.value / rightInteger.value);
    case "*":
      return new Integer(leftInteger.value * rightInteger.value);
    case "-":
      return new Integer(leftInteger.value - rightInteger.value);
    default:
      return evalBooleanInfixExpression(left, op, right);
  }
}

function evalBooleanInfixExpression(left: Maybe<BaseObject>, op: string, right: Maybe<BaseObject>): Maybe<BaseObject> {
  if (!isEqual(left, right)) {
    const leftType = left ?? internal.NULL;
    const rightType = right ?? internal.NULL;
    return typeMismatchError(leftType, rightType, op);
  }
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
      const leftType = left?.type() ?? internal.NULL.type();
      const rightType = right?.type() ?? internal.NULL.type();
      return unknownOperatorError(parseTwoObjectToString(leftType, rightType, op));
  }
}

function evalIfExpression(ifNode: IfExpression): Maybe<BaseObject> {
  const condition = Evaluator(ifNode.condition);
  if (isTruthy(condition)) {
    return Evaluator(ifNode.consequence);
  }
  return Evaluator(ifNode.alternative);
}

function evalProgram(program: Program): Maybe<BaseObject> {
  let result: Maybe<BaseObject> = null;
  for (const statement of program.statements) {
    result = Evaluator(statement);
    if (isExpectObject(result, EBaseObject.RETURN)) {
      return result;
    }

    if (isExpectObject(result, EBaseObject.ERROR)) {
      return result;
    }
  }
  return result;
}

function isExpectObject(object: Maybe<BaseObject>, expectType: EBaseObject): boolean {
  if (!isObject(object)) return false;
  return object?.type() == expectType;
}
function isEqual(objectLeft: Maybe<BaseObject>, objectRight: Maybe<BaseObject>): boolean {
  if (!isObject(objectLeft) || !isObject(objectRight)) return false;
  return objectLeft?.type() == objectRight?.type();
}

function isObject(object: Maybe<BaseObject>): boolean {
  return object !== null;
}
function isBreakObject(object: Maybe<BaseObject>): boolean {
  if (!isObject(object)) return false;
  if (!isExpectObject(object, EBaseObject.RETURN)) return true;
  if (!isExpectObject(object, EBaseObject.ERROR)) return true;

  if (object?.type() == EBaseObject.ERROR) return true;

  return object?.type() == EBaseObject.RETURN;
}
function isTruthy(object: Maybe<BaseObject>): boolean {
  switch (object) {
    case internal.FALSE:
    case internal.NULL:
    case null:
      return false;
    case internal.TRUE:
    default:
      return true;
  }
}
function nativeBooleanObject(input: boolean): InternalBoolean {
  return input ? internal.TRUE : internal.FALSE;
}

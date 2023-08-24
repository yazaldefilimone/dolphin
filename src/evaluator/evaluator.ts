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
  LetStatement,
  Identifier,
  FunctionLiteral,
  Expression,
  CallExpression,
  StringLiteral,
} from "ast";

import {
  BaseObject,
  InternalBoolean,
  Integer,
  internal,
  EBaseObject,
  ReturnObject,
  Environment,
  EnclosedEnvironment,
  BaseString,
} from "evaluator/object";

import { Maybe } from "utils";
import {
  identifierNotFoundError,
  notFunctionError,
  parseTwoObjectToString,
  typeMismatchError,
  unknownIdentifierError,
  unknownOperatorError,
} from "./errors";
import { verifyPermission } from "./permissions";
import { BaseFunction } from "./object/function";

export function Evaluator(node: Maybe<Node>, env: Environment): Maybe<BaseObject> {
  if (node === null) return internal.NULL;
  switch (node.kind) {
    case ProgramKind.program:
      return evalProgram(node as Program, env);
    case StatementKind.BLOCK:
      return evalProgram(node as Program, env);
    case StatementKind.EXPRESSION:
      return Evaluator((node as ExpressionStatement).expression, env);
    case ExpressionKind.INTEGER:
      return new Integer((node as IntegerLiteral).value);
    case ExpressionKind.STRING:
      return new BaseString((node as StringLiteral).value);
    case ExpressionKind.BOOLEAN:
      return nativeBooleanObject((node as BooleanLiteral).value);
    case ExpressionKind.PREFIX:
      const prefixNode = node as PrefixExpression;
      const prefixRight = Evaluator(prefixNode.right, env);
      if (prefixRight === null) return internal.NULL;
      if (isExpectObject(prefixRight, EBaseObject.ERROR)) return prefixRight;
      return evalPrefixExpression(prefixNode.operator, prefixRight);
    case ExpressionKind.INFIX:
      const infix = node as InfixExpression;
      const infixLeft = Evaluator(infix.left, env);
      const infixRight = Evaluator(infix.right, env);
      return evalInfixExpression(infixLeft, infixRight, infix.operator);
    case ExpressionKind.IF:
      const ifNode = node as IfExpression;
      return evalIfExpression(ifNode, env);
    case StatementKind.RETURN:
      const rNode = node as ReturnStatement;
      const returnValue = Evaluator(rNode.returnValue, env);
      if (returnValue === null) return internal.NULL;
      return new ReturnObject(returnValue);
    case StatementKind.LET:
      const letNode = node as LetStatement;
      const exp = Evaluator(letNode.value, env);
      if (isError(exp)) return exp;
      env.setStore(letNode.name.value, exp as BaseObject);
      return null;
    case ExpressionKind.IDENTIFIER:
      const identOrError = evalIdentifierExpression(node as Identifier, env);
      return identOrError;
    case ExpressionKind.FUNCTION:
      return evalFunctionExpression(node as FunctionLiteral, env);
    case ExpressionKind.CALL:
      const callNode = node as CallExpression;
      const func = Evaluator(callNode.function, env);
      if (isError(func)) return func;
      const args = evalExpressions(callNode.arguments, env);
      if (args.length === 1 && isError(args[0])) return args[0];
      if (args === null || func == null) return internal.NULL;
      return applyFunction(func, args);
    default:
      return identifierNotFoundError(node.tokenLiteral());
  }
}

function evaluatorStatements(statements: Statement[], env: Environment): Maybe<BaseObject> {
  let result: Maybe<BaseObject> = null;
  for (const statement of statements) {
    result = Evaluator(statement, env);
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
function evalInfixExpression(left: Maybe<BaseObject>, right: Maybe<BaseObject>, operator: string): Maybe<BaseObject> {
  const leftType = left ?? internal.NULL;
  const rightType = right ?? internal.NULL;
  if (!isEqual(left, right)) {
    return typeMismatchError(leftType, rightType, operator);
  }
  if (!verifyPermission(operator, leftType.type()) && !verifyPermission(operator, rightType.type())) {
    return unknownOperatorError(parseTwoObjectToString(leftType.type(), rightType.type(), operator));
  }
  if (isExpectObject(left, EBaseObject.STRING) && isExpectObject(right, EBaseObject.STRING)) {
    return evalStringInfixExpression(left, right, operator);
  }

  switch (operator) {
    case "+":
    case "/":
    case "*":
    case "-":
      return evalIntegerInfixExpression(left, right, operator);
    case "<":
    case ">":
    case "==":
    case "!=":
      return evalBooleanInfixExpression(left, right, operator);
    default:
      return unknownOperatorError(parseTwoObjectToString(leftType.type(), rightType.type(), operator));
  }
}

function evalIntegerInfixExpression(
  left: Maybe<BaseObject>,
  right: Maybe<BaseObject>,
  operator: string
): Maybe<BaseObject> {
  const leftInteger = left as BaseObject<number>;
  const rightInteger = right as BaseObject<number>;
  switch (operator) {
    case "+":
      return new Integer(leftInteger.value + rightInteger.value);
    case "/":
      return new Integer(leftInteger.value / rightInteger.value);
    case "*":
      return new Integer(leftInteger.value * rightInteger.value);
    case "-":
      return new Integer(leftInteger.value - rightInteger.value);
    default:
      const leftType = left?.type() ?? internal.NULL.type();
      const rightType = right?.type() ?? internal.NULL.type();
      return unknownOperatorError(parseTwoObjectToString(leftType, rightType, operator));
  }
}

function evalBooleanInfixExpression(
  left: Maybe<BaseObject>,
  right: Maybe<BaseObject>,
  operator: string
): Maybe<BaseObject> {
  const l = left as InternalBoolean;
  const r = right as InternalBoolean;
  switch (operator) {
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
      return unknownOperatorError(parseTwoObjectToString(leftType, rightType, operator));
  }
}

function evalStringInfixExpression(
  left: Maybe<BaseObject>,
  right: Maybe<BaseObject>,
  operator: string
): Maybe<BaseObject> {
  let leftType = left?.type() ?? internal.NULL.type();
  let rightType = right?.type() ?? internal.NULL.type();

  if (!isExpectObject(left, EBaseObject.STRING) || !isExpectObject(right, EBaseObject.STRING)) {
    return unknownOperatorError(parseTwoObjectToString(leftType, rightType, operator));
  }
  const leftValue = (left as BaseObject<string>).value;
  const rightValue = (right as BaseObject<string>).value;
  switch (operator) {
    case "+":
      return new BaseString(leftValue + rightValue);
    default:
      return unknownOperatorError(parseTwoObjectToString(leftType, rightType, operator));
  }
}

function evalIfExpression(ifNode: IfExpression, env: Environment): Maybe<BaseObject> {
  const condition = Evaluator(ifNode.condition, env);
  if (isTruthy(condition)) {
    return Evaluator(ifNode.consequence, env);
  }
  return Evaluator(ifNode.alternative, env);
}

function evalProgram(program: Program, env: Environment): Maybe<BaseObject> {
  let result: Maybe<BaseObject> = null;
  for (const statement of program.statements) {
    result = Evaluator(statement, env);
    if (isExpectObject(result, EBaseObject.RETURN)) {
      return result;
    }

    if (isExpectObject(result, EBaseObject.ERROR)) {
      return result;
    }
  }
  return result;
}

function evalIdentifierExpression(node: Identifier, env: Environment): BaseObject {
  const value = env.getStore(node.value);
  if (value === null) {
    return identifierNotFoundError(node.tokenLiteral());
  }
  return value;
}

function evalFunctionExpression(node: FunctionLiteral, env: Environment): BaseObject {
  return new BaseFunction(node.parameters, node.body, env);
}

function evalExpressions(expressions: Maybe<Expression[]>, env: Environment): Maybe<BaseObject>[] {
  if (expressions === null) return [internal.NULL];
  let result: Maybe<BaseObject>[] = [];
  for (const expression of expressions) {
    const object = Evaluator(expression, env);
    if (isError(object)) {
      return [object];
    }
    result.push(object);
  }
  if (result === null) return [internal.NULL];
  return result;
}

function applyFunction(fn: BaseObject, args: Maybe<BaseObject>[]): Maybe<BaseObject> {
  if (!isExpectObject(fn, EBaseObject.FUNCTION)) {
    return notFunctionError(fn.type());
  }
  const baseFunction = fn as BaseFunction;
  let extendEnv = extendFunctionEnv(baseFunction, args);
  const evaluateBody = Evaluator(baseFunction.body, extendEnv);
  // return unwrapReturnValue(evaluateBody);
  return evaluateBody;
}

function extendFunctionEnv(fun: BaseFunction, args: Maybe<BaseObject>[]): Environment {
  const env = new EnclosedEnvironment(fun.env);
  fun.parameters?.forEach((parameter, index) => {
    if (args == null) {
      throw new Error("Internal Error...");
    }
    let arg = args[index];
    arg && env.setStore(parameter.value, arg);
  });
  return env;
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
  return object?.type() == EBaseObject.RETURN;
}
function isError(object: Maybe<BaseObject>): boolean {
  if (isExpectObject(object, EBaseObject.ERROR)) {
    return true;
  }
  return false;
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

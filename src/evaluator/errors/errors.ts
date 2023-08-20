import { BaseObject, EBaseObject, InternalError } from "evaluator/object";

export function unknownOperatorError(operator: string): InternalError {
  return new InternalError(`Unknown operator:  ${operator}`);
}

export function typeMismatchError(left: BaseObject, right: BaseObject, operator: string): InternalError {
  return new InternalError(`Type mismatch: ${left.type()} ${operator} ${right.type()}`);
}

export function unknownIdentifierError(name: string): InternalError {
  return new InternalError(`Unknown identifier: ${name}`);
}

export function notFunctionError(name: string): InternalError {
  return new InternalError(`Not a function: ${name}`);
}
export function identifierNotFoundError(name: string): InternalError {
  return new InternalError(`identifier not found: ${name}`);
}

export function parseTwoObjectToString(left: EBaseObject, right: EBaseObject, operator: string): string {
  if (!left && right) {
    return `${operator} ${right}`;
  }
  if (!right && left) {
    return `${left} ${operator}`;
  }
  if (left && right) {
    return `${left} ${operator} ${right}`;
  }
  throw new Error("left and right are null");
}

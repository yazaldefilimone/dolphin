import { EBaseObject } from "evaluator/object";
export type Permission = "==" | "!=" | "<" | ">" | "+" | "-" | "*" | "/" | "!";

const PermissionOperator = new Map<Permission, Set<EBaseObject>>();

export const SetPermission = (operator: Permission, permission: EBaseObject) => {
  if (!PermissionOperator.has(operator)) {
    PermissionOperator.set(operator, new Set<EBaseObject>());
  }
  PermissionOperator.get(operator)?.add(permission);
};
SetPermission("<", EBaseObject.INTEGER);
SetPermission("<", EBaseObject.BOOLEAN);
SetPermission(">", EBaseObject.INTEGER);
SetPermission(">", EBaseObject.BOOLEAN);
SetPermission("==", EBaseObject.INTEGER);
SetPermission("==", EBaseObject.BOOLEAN);
SetPermission("!=", EBaseObject.INTEGER);
SetPermission("!=", EBaseObject.BOOLEAN);
SetPermission("+", EBaseObject.INTEGER);
SetPermission("-", EBaseObject.INTEGER);
SetPermission("*", EBaseObject.INTEGER);
SetPermission("/", EBaseObject.INTEGER);
SetPermission("!", EBaseObject.BOOLEAN);

export const verifyPermission = (operator: string, objectType: EBaseObject): boolean => {
  return Boolean(PermissionOperator.get(operator as Permission)?.has(objectType));
};

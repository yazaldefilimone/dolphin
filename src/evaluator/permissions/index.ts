import { EBaseObject } from "evaluator/object";
export type Operator = "==" | "!=" | "<" | ">" | "+" | "-" | "*" | "/" | "!";

const PermissionTable = new Map<Operator, Set<EBaseObject>>();

export const setPermission = (operator: Operator, permissions: EBaseObject[]) => {
  if (!PermissionTable.has(operator)) {
    PermissionTable.set(operator, new Set<EBaseObject>());
  }
  permissions.forEach((permission) => {
    PermissionTable.get(operator)?.add(permission);
  });
};
setPermission("<", [EBaseObject.INTEGER, EBaseObject.BOOLEAN]);
setPermission(">", [EBaseObject.INTEGER, EBaseObject.BOOLEAN]);
setPermission("==", [EBaseObject.INTEGER, EBaseObject.BOOLEAN]);
setPermission("!=", [EBaseObject.INTEGER, EBaseObject.BOOLEAN]);
setPermission("+", [EBaseObject.INTEGER, EBaseObject.STRING]);
setPermission("-", [EBaseObject.INTEGER]);
setPermission("*", [EBaseObject.INTEGER]);
setPermission("/", [EBaseObject.INTEGER]);
setPermission("!", [EBaseObject.BOOLEAN]);

export const verifyPermission = (operator: string, objectType: EBaseObject): boolean => {
  return Boolean(PermissionTable.get(operator as Operator)?.has(objectType));
};

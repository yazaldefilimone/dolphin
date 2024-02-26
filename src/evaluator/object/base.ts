export enum EBaseObject {
  INTEGER = "INTEGER",
  BOOLEAN = "BOOLEAN",
  RETURN = "RETURN",
  NULL = "NULL",
  STRING = "STRING",
  ERROR = "ERROR",
  FUNCTION = "FUNCTION",
  BUILTIN = "BUILTIN",
  ARRAY = "ARRAY",
}
export interface BaseObject<T = unknown> {
  type(): EBaseObject;
  inspect(): string;
  get value(): T;
}

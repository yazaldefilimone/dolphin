export enum EBaseObject {
  INTEGER = "INTEGER",
  BOOLEAN = "BOOLEAN",
  RETURN = "RETURN",
  NULL = "NULL",
}
export interface BaseObject<T = unknown> {
  type(): EBaseObject;
  inspect(): string;
  get value(): T;
}

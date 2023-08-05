export enum objectType {
  INTEGER = "INTEGER",
  BOOLEAN = "BOOLEAN",
  NULL = "NULL",
}
export interface BaseObject<T = unknown> {
  type(): objectType;
  inspect(): string;
  get value(): T;
}

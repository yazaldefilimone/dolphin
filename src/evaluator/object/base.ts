export enum objectType {
  INTEGER = "INTEGER",
  BOOLEAN = "BOOLEAN",
  NULL = "NULL",
}
export interface BaseObject {
  type(): objectType;
  inspect(): string;
}

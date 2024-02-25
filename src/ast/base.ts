export enum ProgramKind {
  program = "program",
}
export enum StatementKind {
  LET = "LET",
  RETURN = "RETURN",
  EXPRESSION = "EXPRESSION",
  BLOCK = "BLOCK",
}
export enum ExpressionKind {
  IDENTIFIER = "IDENTIFIER",
  INTEGER = "INTEGER",
  ARRAY = "ARRAY",
  BOOLEAN = "BOOLEAN",
  STRING = "STRING",
  PREFIX = "PREFIX",
  INFIX = "INFIX",
  IF = "IF",
  FUNCTION = "FUNCTION",
  CALL = "CALL",
}

type nodeKindType = StatementKind | ExpressionKind | ProgramKind;

export interface Node {
  kind: nodeKindType;
  tokenLiteral(): string;
  toString(): string;
}

export interface Statement extends Node {
  kind: StatementKind | ProgramKind;
  statementNode(): void;
}

export interface Expression extends Node {
  kind: ExpressionKind;
  expressionNode(): void;
}

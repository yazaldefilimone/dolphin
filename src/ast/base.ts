export interface Node {
  tokenLiteral(): string;
  toString(): string;
}

export interface Statement extends Node {
  statementNode(): void;
}

export interface Expression extends Node {
  expressionNode(): void;
}

export type parseResult<T> = T | null;

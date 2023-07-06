import { Token } from "token";

export interface INode {
  tokenLiteral(): string;
}

export interface IExpression extends INode {
  expressionNode(): unknown;
}

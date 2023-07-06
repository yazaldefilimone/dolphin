import { INode } from "ast/node";

export interface IExpression extends INode {
  expressionNode(): string;
}

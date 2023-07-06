import { INode } from "ast/node";

export interface IStatement extends INode {
  statementNode(): string;
}

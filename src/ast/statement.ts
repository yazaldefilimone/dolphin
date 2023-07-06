import { INode } from "ast/type";

export interface IStatement extends INode {
  statementNode(): unknown;
}

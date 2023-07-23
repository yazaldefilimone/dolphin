import { Token } from "token";
import { Expression } from "./base";

export class InfixExpression implements Expression {
  private token: Token;
  left: Expression | null = null;
  operator: string;
  right: Expression | null = null;
  constructor(token: Token, operator: string) {
    this.token = token;
    this.operator = operator;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }

  toString(): string {
    let leftString = "";
    if (this.left !== null) {
      leftString = this.left.toString();
    }
    let rightString = "";
    if (this.right !== null) {
      rightString = this.right.toString();
    }
    const out = `(${leftString} ${this.operator} ${rightString})`;
    return out;
  }

  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
}

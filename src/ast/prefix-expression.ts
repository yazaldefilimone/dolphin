import { Token } from "token";
import { Expression } from "./base";

export class PrefixExpression implements Expression {
  token: Token;
  operator: string;
  right: Expression;
  constructor(token: Token) {
    this.token = token;
  }

  tokenLiteral(): string {
    throw new Error("Method not implemented.");
  }
  toString(): string {
    const rightString = this.right.toString();
    const out = `(${this.operator}${rightString})`;
    return out;
  }
  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
}

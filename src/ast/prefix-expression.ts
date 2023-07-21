import { Token } from "token";
import { Expression } from "./base";

export class PrefixExpression implements Expression {
  private token: Token;
  private operator: string;
  right: Expression | null = null;
  constructor(token: Token, operator: string) {
    this.token = token;
    this.operator = operator;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
  toString(): string {
    let rightString = "";
    if (this.right !== null) {
      rightString = this.right.toString();
    }
    const out = `(${this.operator}${rightString})`;
    return out;
  }
  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
}

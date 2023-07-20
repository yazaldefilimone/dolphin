import { Token } from "token";
import { Expression } from "./base";

export class IntegerLiteral implements Expression {
  private token: Token;
  public value: number;
  constructor(token: Token) {
    this.token = token;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
  toString(): string {
    return this.token.literal;
  }
  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
}

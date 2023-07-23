import { Token } from "token";
import { Expression } from "./base";

export class BooleanLiteral implements Expression {
  private token: Token;
  value: boolean;
  constructor(token: Token, value: boolean) {
    this.token = token;
    this.value = value;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
  toString(): string {
    return this.tokenLiteral();
  }
  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
}

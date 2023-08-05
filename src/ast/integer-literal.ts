import { Token } from "token";
import { Expression, ExpressionKind } from "./base";

export class IntegerLiteral implements Expression {
  private token: Token;
  public value: number;
  kind: ExpressionKind;
  constructor(token: Token) {
    this.token = token;
    this.kind = ExpressionKind.INTEGER;
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

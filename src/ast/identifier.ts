import { Token } from "token";
import { Expression, ExpressionKind } from "./base";

export class Identifier implements Expression {
  private token: Token;
  public value: string;
  kind: ExpressionKind;
  constructor(token: Token) {
    this.token = token;
    this.kind = ExpressionKind.IDENTIFIER;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }

  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
  toString(): string {
    return this.value;
  }
}

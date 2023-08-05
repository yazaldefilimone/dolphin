import { Token } from "token";
import { Expression, ExpressionKind } from "./base";

export class BooleanLiteral implements Expression {
  private token: Token;
  value: boolean;
  kind: ExpressionKind;
  constructor(token: Token, value: boolean) {
    this.token = token;
    this.value = value;
    this.kind = ExpressionKind.BOOLEAN;
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

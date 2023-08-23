import { Expression, ExpressionKind } from "ast";
import { Token } from "token";

export class StringLiteral implements Expression {
  value: string;
  kind: ExpressionKind;
  token: Token;
  constructor(token: Token, value: string) {
    this.value = value;
    this.kind = ExpressionKind.STRING;
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

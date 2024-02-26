import { Token } from "token/token";
import { Expression, ExpressionKind } from "./base";
import { Maybe } from "utils";

export class ArrayLiteral implements Expression {
  token: Token;
  kind: ExpressionKind;
  public elements: Maybe<Expression[]>;
  constructor(token: Token, elements: Maybe<Expression[]> = null) {
    this.token = token;
    this.kind = ExpressionKind.ARRAY;
    this.elements = elements || null;
  }
  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
  tokenLiteral(): string {
    return this.token.literal;
  }
  toString(): string {
    if (this.elements === null) {
      return "";
    }
    return `[${this.elements.map((e) => e.toString()).join(", ")}]`;
  }
}

import { Token } from "token";
import { Expression, ExpressionKind } from "./base";
import { Maybe } from "utils";

export class PrefixExpression implements Expression {
  private token: Token;
  operator: string;
  right: Maybe<Expression> = null;
  kind: ExpressionKind;
  constructor(token: Token, operator: string) {
    this.token = token;
    this.operator = operator;
    this.kind = ExpressionKind.PREFIX;
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

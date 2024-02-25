import { Token } from "token";
import { Expression, ExpressionKind } from "./base";
import { Maybe } from "utils";

export class IndexExpression implements Expression {
  token: Token;
  left: Maybe<Expression>;
  index: Maybe<Expression>;
  kind: ExpressionKind;
  constructor(token: Token, left: Maybe<Expression> = null, index: Maybe<Expression> = null) {
    this.token = token;
    this.left = left;
    this.index = index;
    this.kind = ExpressionKind.INDEX;
  }

  expressionNode() {}
  tokenLiteral() {
    return this.token.literal;
  }
  toString() {
    return `(${this.left?.toString()}[${this.index?.toString()}])`;
  }
}

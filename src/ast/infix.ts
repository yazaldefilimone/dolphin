import { Token } from "token";
import { Expression, ExpressionKind } from "./base";
import { Maybe } from "utils";

export class InfixExpression implements Expression {
  private token: Token;
  left: Maybe<Expression> = null;
  operator: string;
  right: Maybe<Expression> = null;
  kind: ExpressionKind;
  constructor(token: Token, operator: string) {
    this.token = token;
    this.operator = operator;
    this.kind = ExpressionKind.INFIX;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }

  toString(): string {
    let leftString = "";
    if (this.left !== null) {
      leftString = this.left.toString();
    }
    let rightString = "";
    if (this.right !== null) {
      rightString = this.right.toString();
    }
    const out = `(${leftString} ${this.operator} ${rightString})`;
    return out;
  }

  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
}

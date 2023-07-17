import { Expression, Statement } from "ast";
import { Token } from "token";

export class ExpressionStatement implements Statement {
  token: Token;
  expression: Expression;
  constructor(token: Token) {
    this.token = token;
  }
  tokenLiteral() {
    return this.token.literal;
  }
  statementNode() {
    throw new Error("Method not implemented.");
  }
  toString(): string {
    if (this.expression !== null) {
      return this.expression.toString();
    }
    return "";
  }
}

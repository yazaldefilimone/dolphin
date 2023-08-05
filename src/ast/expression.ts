import { Expression, ProgramKind, Statement, StatementKind } from "ast";
import { Token } from "token";
import { Maybe } from "utils";

export class ExpressionStatement implements Statement {
  token: Token;
  expression: Maybe<Expression>;
  kind: StatementKind;
  constructor(token: Token) {
    this.token = token;
    this.kind = StatementKind.EXPRESSION;
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

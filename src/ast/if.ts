import { Expression } from "ast";
import { Token } from "token";
import { BlockStatement } from "ast";

export class IfExpression implements Expression {
  private token: Token;
  condition: Expression;
  consequence: BlockStatement;
  alternative: BlockStatement;
  constructor(token: Token) {
    this.token = token;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
  toString(): string {
    throw new Error("Method not implemented.");
  }

  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
}

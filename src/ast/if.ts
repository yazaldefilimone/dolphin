import { Expression } from "ast";
import { Token } from "token";
import { BlockStatement } from "ast";
import { concatenationOfString } from "utils";

export class IfExpression implements Expression {
  private token: Token;
  condition: Expression | null = null;
  consequence: BlockStatement | null = null;
  alternative: BlockStatement | null = null;
  constructor(token: Token) {
    this.token = token;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
  toString(): string {
    const concat = concatenationOfString("if");
    if (this.condition !== null) {
      concat.plus([" ", this.condition.toString()]);
    }
    if (this.consequence !== null) {
      concat.plus([" ", this.consequence.toString()]);
    }
    if (this.alternative !== null) {
      concat.plus([" ", "else", " ", this.alternative.toString()]);
    }
    return concat.get();
  }

  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
}

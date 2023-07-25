import { Expression } from "ast";
import { Token } from "token";
import { BlockStatement } from "ast";

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
    let out = "if";
    if (this.condition !== null) {
      out = out.concat(" ", this.condition.toString());
    }
    if (this.consequence !== null) {
      out = out.concat(" ", this.consequence.toString());
    }
    if (this.alternative !== null) {
      out = out.concat(" ", "else", " ", this.alternative.toString());
    }
    return out;
  }

  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
}

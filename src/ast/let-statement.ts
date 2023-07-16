import { Token } from "token";
import { Expression, Statement } from "./base";
import { Identifier } from "./identifier";

export class LetStatement implements Statement {
  token: Token;
  name: Identifier;
  value: Expression;
  constructor(token: Token) {
    this.token = token;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }

  statementNode(): void {
    throw new Error("Method not implemented.");
  }
}

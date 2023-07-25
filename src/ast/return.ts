import { Token } from "token";
import { Expression, Statement } from "./base";

export class ReturnStatement implements Statement {
  token: Token;
  returnValue: Expression | null;
  constructor(token: Token) {
    this.token = token;
    this.returnValue = null;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }

  toString(): string {
    let out_string = this.tokenLiteral().concat(" ");
    if (this.returnValue !== null) {
      out_string = out_string.concat(this.returnValue.toString());
    }
    return out_string.concat(";");
  }

  statementNode(): void {
    throw new Error("Method not implemented.");
  }
}

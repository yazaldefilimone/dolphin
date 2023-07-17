import { Token } from "token";
import { Expression, Statement } from "./base";
import { Identifier } from "./identifier";

export class LetStatement implements Statement {
  token: Token;
  name: Identifier;
  value: Expression | null;
  constructor(token: Token) {
    this.token = token;
    this.value = null;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }

  statementNode(): void {
    throw new Error("Method not implemented.");
  }
  toString(): string {
    const out_string = this.tokenLiteral().concat(" ");
    out_string.concat(this.name.toString()).concat(" = ");
    if (this.value !== null) {
      console.log(this.value);
      out_string.concat(this.value.toString());
    }
    return out_string.concat(";");
  }
}

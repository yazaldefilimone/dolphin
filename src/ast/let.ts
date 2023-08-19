import { Token } from "token";
import { Expression, ProgramKind, Statement, StatementKind } from "./base";
import { Identifier } from "./identifier";
import { Maybe, concatenationOfString } from "utils";

export class LetStatement implements Statement {
  token: Token;
  name: Identifier;
  value: Maybe<Expression> = null;
  kind: StatementKind;
  constructor(token: Token) {
    this.token = token;
    this.kind = StatementKind.LET;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }

  statementNode(): void {
    throw new Error("Method not implemented.");
  }
  toString(): string {
    const out_string = concatenationOfString(this.tokenLiteral());
    out_string.plus([" ", this.name.toString()]);
    if (this.value !== null) {
      out_string.plus([" ", "=", " ", this.value.toString(), ";"]);
    }
    return out_string.get();
  }
}

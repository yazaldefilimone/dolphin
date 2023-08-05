import { Token } from "token";
import { Expression, ProgramKind, Statement, StatementKind } from "./base";
import { Identifier } from "./identifier";
import { Maybe } from "utils";

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
    let out_string = this.tokenLiteral().concat(" ");
    out_string = out_string.concat(this.name.toString());
    if (this.value !== null) {
      out_string = out_string.concat(" ", "=", " ");
      out_string = out_string.concat(this.value.toString());
    }
    return out_string.concat(";");
  }
}

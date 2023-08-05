import { Token } from "token";
import { Expression, ProgramKind, Statement, StatementKind } from "./base";
import { Maybe } from "utils";

export class ReturnStatement implements Statement {
  token: Token;
  returnValue: Maybe<Expression>;
  kind: StatementKind;
  constructor(token: Token) {
    this.token = token;
    this.returnValue = null;
    this.kind = StatementKind.RETURN;
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

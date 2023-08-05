import { Token } from "token";
import { Statement, StatementKind } from "ast";

export class BlockStatement implements Statement {
  token: Token; // the { token
  statements: Statement[]; // the statements in the block
  kind: StatementKind;
  constructor(token: Token) {
    this.token = token;
    this.statements = [];
    this.kind = StatementKind.BLOCK;
  }
  tokenLiteral(): string {
    return this.token.literal;
  }
  toString(): string {
    const out = this.statements.reduce((acc, statement) => {
      return acc + statement.toString();
    }, "");
    return `{${out}}`;
  }
  statementNode(): void {
    throw new Error("Method not implemented.");
  }
}

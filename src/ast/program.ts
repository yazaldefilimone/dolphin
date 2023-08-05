import { Statement, ProgramKind } from "./base";

export class Program implements Statement {
  statements: Statement[];
  kind: ProgramKind.program;
  constructor() {
    this.statements = [];
    this.kind = ProgramKind.program;
  }

  statementNode(): void {
    throw new Error("Method not implemented.");
  }

  tokenLiteral(): string {
    if (this.statements.length > 0) {
      return this.statements[0].tokenLiteral();
    }
    return "";
  }
  toString(): string {
    const statementInString = this.statements.reduce((acc, stmt) => {
      return acc + stmt.toString();
    }, "");
    return statementInString;
  }
}

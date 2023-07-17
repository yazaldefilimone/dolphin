import { Statement } from "./base";

export class Program implements Statement {
  statements: Statement[];
  constructor() {
    this.statements = [];
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
    const statementInString = this.statements.map((stmt) => stmt.toString());
    return statementInString.join("");
  }
}

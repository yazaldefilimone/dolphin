import { IStatement } from "ast/statement";

export class Program {
  public statements: IStatement[] = [];

  public tokenLiteral(): string {
    if (this.statements.length > 0) {
      return this.statements[0].tokenLiteral();
    }
    return "";
  }
}

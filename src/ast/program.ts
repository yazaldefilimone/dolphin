import { IStatement } from "ast/statement";

export class Program {
  private statements: IStatement[] = [];

  public tokenLiteral(): string {
    if (this.statements.length > 0) {
      return this.statements[0].TokenLiteral();
    }
    return "";
  }
}

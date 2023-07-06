import { IStatement } from "ast/statement";
import { Token } from "token";
import { IExpression } from "ast/type";

export class VariableStatement implements IStatement {
  constructor(public token: Token) {}
  tokenLiteral() {
    return this.token.literal;
  }
  statementNode() {}
}

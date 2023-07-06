import { Token } from "token";
import { IExpression } from "ast/expression";

export interface ILetStatement {
  token: Token;
  name: Identifier;
  value: IExpression;
}

export class Identifier {
  token: Token;
  value: string;
  expressionNode() {}

  tokenLiteral() {
    return this.token.literal;
  }
}

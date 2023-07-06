import { Token } from "token";

export class Identifier {
  token: Token;
  value: string;
  expressionNode() {}

  tokenLiteral() {
    return this.token.literal;
  }
}

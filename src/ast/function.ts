import { Token } from "token";
import { Expression, ExpressionKind, Identifier, BlockStatement } from "ast";
import { Maybe, concatenationOfString } from "utils";

export class FunctionLiteral implements Expression {
  private token: Token;
  parameters: Maybe<Identifier[]> = null;
  body: Maybe<BlockStatement> = null;
  kind: ExpressionKind;
  constructor(token: Token) {
    this.token = token;
    this.kind = ExpressionKind.FUNCTION;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }

  toString(): string {
    const concat = concatenationOfString("");

    const parameters = this.parameters?.reduce((initial, parameter) => {
      if (initial) {
        return initial.concat(",", parameter.toString());
      }
      return initial.concat(parameter.toString());
    }, "");

    concat.plus(this.tokenLiteral());
    concat.plus("(");
    concat.plus(parameters ?? "");
    concat.plus(") ");
    concat.plus(this.body?.toString() ?? "");

    return concat.get();
  }
  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
}

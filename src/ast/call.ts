import { Token } from "token";
import { Expression, ExpressionKind } from "ast";
import { Maybe, concatenationOfString } from "utils";

export class CallExpression implements Expression {
  token: Token;
  arguments: Maybe<Expression[]> = null;
  function: Expression;
  kind: ExpressionKind;
  constructor(token: Token) {
    this.token = token;
    this.kind = ExpressionKind.CALL;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
  toString(): string {
    const concat = concatenationOfString("");
    const args = this.arguments?.reduce((acc, arg) => {
      if (acc) {
        return acc.concat(",", arg.toString());
      }
      return acc.concat(arg.toString());
    }, "");
    concat.plus(this.function.toString());
    concat.plus("(");
    concat.plus(args ?? "");
    concat.plus(")");
    return concat.get();
  }
  expressionNode(): void {
    throw new Error("Method not implemented.");
  }
}

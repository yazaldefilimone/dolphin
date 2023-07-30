import { Token } from "token";
import { Expression } from "./base";
import { Identifier } from "./identifier";
import { BlockStatement } from "./block";
import { concatenationOfString } from "utils";
export class FunctionLiteral implements Expression {
  private token: Token;
  parameters: Identifier[] | null = null;
  body: BlockStatement | null = null;
  constructor(token: Token) {
    this.token = token;
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

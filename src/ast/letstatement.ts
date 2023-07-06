import { Token } from "token";
import { IExpression } from "ast/type";
import { Identifier } from "./identifier";

export class LetStatement {
  token: Token;
  name: Identifier;
  value: IExpression;
}

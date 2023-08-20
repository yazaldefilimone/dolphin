import { BlockStatement, Identifier } from "ast";
import { BaseObject, EBaseObject, Environment } from "evaluator/object";
import { Maybe, concatenationOfString } from "utils";

export class BaseFunction implements BaseObject {
  public parameters: Maybe<Identifier[]>;
  public body: Maybe<BlockStatement>;
  public env: Environment;
  constructor(parameters: Maybe<Identifier[]>, body: Maybe<BlockStatement>, env: Environment) {
    this.parameters = parameters;
    this.body = body;
    this.env = env;
  }

  public type(): EBaseObject {
    return EBaseObject.FUNCTION;
  }

  public inspect(): string {
    const concat = concatenationOfString("fn(");
    const params = this.parameters?.reduce((acc, cur) => {
      if (!acc) {
        acc += ", ";
      }
      acc += cur.value;
      return acc;
    }, "");
    params && concat.plus(params);
    concat.plus(")");
    this.body && concat.plus(this.body.toString());
    return concat.get();
  }

  public get value(): this {
    return this;
  }
}

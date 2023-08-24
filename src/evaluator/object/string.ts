import { BaseObject, EBaseObject } from "evaluator/object";

export class BaseString implements BaseObject<string> {
  private _value: string;
  constructor(value: string) {
    this._value = value;
  }
  type(): EBaseObject {
    return EBaseObject.STRING;
  }
  inspect(): string {
    return this._value;
  }
  get value(): string {
    return this._value;
  }
}

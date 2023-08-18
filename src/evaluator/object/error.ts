import { BaseObject, EBaseObject } from "./base";

export class InternalError implements BaseObject<string> {
  private _value: string;
  constructor(value: string) {
    this._value = value;
  }
  type(): EBaseObject {
    return EBaseObject.ERROR;
  }
  inspect(): string {
    return `ERROR: ${this._value}`;
  }
  get value() {
    return this._value;
  }
}

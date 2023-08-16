import { BaseObject, EBaseObject } from "evaluator/object";

export class InternalBoolean implements BaseObject<boolean> {
  private _value: boolean;
  constructor(value: boolean) {
    this._value = value;
  }
  type(): EBaseObject {
    return EBaseObject.BOOLEAN;
  }
  inspect(): string {
    return this._value.toString();
  }
  get value() {
    return this._value;
  }
}

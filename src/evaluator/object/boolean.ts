import { BaseObject, objectType } from "evaluator/object";

export class Boolean implements BaseObject<boolean> {
  private _value: boolean;
  constructor(value: boolean) {
    this._value = value;
  }
  type(): objectType {
    return objectType.BOOLEAN;
  }
  inspect(): string {
    return this._value.toString();
  }
  get value() {
    return this._value;
  }
}

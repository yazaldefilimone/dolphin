import { BaseObject, objectType } from "evaluator/object";

export class Integer implements BaseObject<number> {
  private _value: number;
  constructor(value: number) {
    this._value = value;
  }

  type(): objectType {
    return objectType.INTEGER;
  }
  inspect(): string {
    return this._value.toString();
  }
  get value() {
    return this._value;
  }
}

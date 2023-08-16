import { BaseObject, EBaseObject } from "evaluator/object";

export class ReturnObject<T = unknown> implements BaseObject<T> {
  constructor(private _value: BaseObject<T>) {
    this._value = _value;
  }
  type(): EBaseObject {
    return EBaseObject.RETURN;
  }
  inspect(): string {
    return this._value.inspect();
  }
  get value() {
    return this._value.value;
  }
}

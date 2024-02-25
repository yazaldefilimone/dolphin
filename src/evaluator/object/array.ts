import { BaseObject, EBaseObject } from "evaluator/object";

export class InternalArray implements BaseObject<BaseObject[]> {
  private _elements: BaseObject[];
  constructor(elements: BaseObject[]) {
    this._elements = elements;
  }
  type(): EBaseObject {
    return EBaseObject.ARRAY;
  }
  inspect(): string {
    return this._elements.map((el) => el.inspect()).join(", ");
  }
  get value(): BaseObject[] {
    return this._elements;
  }
}

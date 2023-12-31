import { BaseObject, EBaseObject } from "evaluator/object";

export class InternalNull implements BaseObject<null> {
  type(): EBaseObject {
    return EBaseObject.NULL;
  }
  inspect(): string {
    return "nil";
  }
  get value() {
    return null;
  }
}

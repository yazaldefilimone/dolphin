import { BaseObject, objectType } from "evaluator/object";

export class InternalNull implements BaseObject<null> {
  type(): objectType {
    return objectType.NULL;
  }
  inspect(): string {
    return "null";
  }
  get value() {
    return null;
  }
}

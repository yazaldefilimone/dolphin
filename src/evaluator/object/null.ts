import { BaseObject, objectType } from "evaluator/object";

export class Null implements BaseObject<null> {
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

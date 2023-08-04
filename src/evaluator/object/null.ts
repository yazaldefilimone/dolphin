import { BaseObject, objectType } from "evaluator/object";

export class Null implements BaseObject {
  type(): objectType {
    return objectType.NULL;
  }
  inspect(): string {
    return "null";
  }
}

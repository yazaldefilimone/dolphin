import { BaseObject, objectType } from "evaluator/object";

export class Boolean implements BaseObject {
  private value: boolean;
  constructor(value: boolean) {
    this.value = value;
  }
  type(): objectType {
    return objectType.BOOLEAN;
  }
  inspect(): string {
    return this.value.toString();
  }
}

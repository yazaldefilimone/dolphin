import { BaseObject, objectType } from "evaluator/object";

export class Integer implements BaseObject {
  private value: number;
  constructor(value: number) {
    this.value = value;
  }

  type(): objectType {
    return objectType.INTEGER;
  }
  inspect(): string {
    return this.value.toString();
  }
}

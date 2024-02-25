import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { InternalArray, Integer } from "evaluator/object";

describe("Evaluator", () => {
  describe("Array", () => {
    it("should create array", () => {
      const arrayTest = [1, 2, 3];
      const tt = {
        input: `
          let myArray = [${arrayTest.join(", ")}];
          myArray;
        `,
        expected: new InternalArray(arrayTest.map((num) => new Integer(num))),
      };
      const sut = makeSut(tt.input);
      expect(sut).not.toBeNull();
      expect(sut?.value).toEqual(tt.expected.value);
    });
  });
});

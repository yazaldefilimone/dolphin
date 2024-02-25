import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { InternalArray, Integer } from "evaluator/object";

describe("Evaluator", () => {
  describe("Array", () => {
    it("should create array and access by index", () => {
      const tests = [
        {
          input: `
        [1, 2, 3][0];
        `,
          expected: 1,
        },
        {
          input: `
        [1, 2, 3][1];
        `,
          expected: 2,
        },
        {
          input: `
        [1, 2, 3][2];
        `,
          expected: 3,
        },
        {
          input: `
        let i = 0;
        [1][i];
        `,
          expected: 1,
        },
        {
          input: `
        [1, 2, 3][1 + 1];
        `,
          expected: 3,
        },
        {
          input: `
        let myArray = [1, 2, 3];
        myArray[2];
        `,
          expected: 3,
        },
        {
          input: `
        let myArray = [1, 2, 3];
        myArray[0] + myArray[1] + myArray[2];
        `,
          expected: 6,
        },
        {
          input: `
        let myArray = [1, 2, 3];
        let i = myArray[0];
        myArray[i];
        `,
          expected: 2,
        },
        {
          input: `
        [1, 2, 3][3];
        `,
          expected: null,
        },
        {
          input: `
        [1, 2, 3][-1];
        `,
          expected: null,
        },
      ];
      for (const tt of tests) {
        const sut = makeSut(tt.input);
        expect(sut).not.toBeNull();
        expect(sut?.value).toBe(tt.expected);
      }
    });
  });
});

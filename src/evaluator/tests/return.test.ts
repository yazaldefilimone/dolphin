import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";

describe("Evaluator", () => {
  it("return", () => {
    const tests = [
      {
        input: "return 10;",
        expected: 10,
      },

      {
        input: "return 2 * 5; 8;",
        expected: 10,
      },
      {
        input: "9; return 2 * 5;",
        expected: 10,
      },
    ];

    tests.forEach((tt) => {
      const evaluated = makeSut(tt.input);
      expect(evaluated).not.toBeNull();
      console.log(evaluated?.inspect());
      expect(evaluated?.value).toEqual(tt.expected);
    });
  });
  it("return inside of if", () => {
    const tests = [
      {
        input: `
        if (1 == 1) {
          if (1 != 1) {
            return 10;
          }
         return 100;
        }
        return 1000;
        `,
        expected: 100,
      },
      {
        input: `
        if (10 > 1) {
          if (10 > 1) {
             return 10;
          }
          return 1;
        }
        return 1;
      `,
        expected: 10,
      },
      {
        input: `
        if (false) {
          return 1;
        }
        return -1;
      `,
        expected: -1,
      },
    ];

    tests.forEach((tt) => {
      tests.forEach((tt) => {
        const evaluated = makeSut(tt.input);
        expect(evaluated).not.toBeNull();
        console.log(evaluated?.inspect());
        expect(evaluated?.value).toEqual(tt.expected);
      });
    });
  });
});

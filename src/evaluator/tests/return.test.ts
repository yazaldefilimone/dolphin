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
        input: "return 2 * 5;",
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
});

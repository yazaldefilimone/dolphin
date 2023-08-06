import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";

describe("Evaluator", () => {
  it("bang operator", () => {
    const tests = [
      {
        input: "!true",
        expected: false,
      },
      {
        input: "!false",
        expected: true,
      },
    ];

    tests.forEach((tt) => {
      const evaluated = makeSut(tt.input);
      expect(evaluated).not.toBeNull();
      expect(evaluated?.value).toEqual(tt.expected);
    });
  });
});

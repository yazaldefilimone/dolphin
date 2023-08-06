import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";

describe("Evaluator", () => {
  it("evaluator boolean literal", () => {
    const tests = [
      {
        input: "true",
        expected: true,
      },
      {
        input: "false",
        expected: false,
      },
    ];

    tests.forEach((tt) => {
      const evaluated = makeSut(tt.input);
      expect(evaluated?.value).toEqual(tt.expected);
    });
  });
});

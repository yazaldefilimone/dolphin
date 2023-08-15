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
      {
        input: "false != false",
        expected: false,
      },
      {
        input: "true != false",
        expected: true,
      },
      {
        input: "false == false",
        expected: true,
      },
      {
        input: `(1 < 2) == true`,
        expected: true,
      },
      {
        input: `(1 < 2) == false`,
        expected: false,
      },
      {
        input: `(1 > 2) == true`,
        expected: false,
      },
    ];

    tests.forEach((tt) => {
      const evaluated = makeSut(tt.input);
      expect(evaluated?.value).toEqual(tt.expected);
    });
  });
});

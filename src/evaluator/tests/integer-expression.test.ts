import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";

describe("Evaluator", () => {
  it("evaluator integer expression", () => {
    const tests = [
      {
        input: "5",
        expected: 5,
      },
      {
        input: "10",
        expected: 10,
      },
    ];

    tests.forEach((tt) => {
      const evaluated = makeSut(tt.input);
      expect(evaluated?.value).toEqual(tt.expected);
    });
  });
});

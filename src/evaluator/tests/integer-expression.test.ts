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
      const evaluated = makeSut<number>(tt.input);
      expect(evaluated?.value).toEqual(tt.expected);
    });
  });
  it("evaluator integer with prefix expression", () => {
    const tests = [
      {
        input: "-40",
        expected: -40,
      },
      {
        input: "+20",
        expected: +20,
      },
    ];

    tests.forEach((tt) => {
      const evaluated = makeSut<number>(tt.input);
      expect(evaluated?.value).toEqual(tt.expected);
    });
  });
});

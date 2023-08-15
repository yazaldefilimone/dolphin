import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";

describe("Evaluate", () => {
  it("should evaluate a simple expression", () => {
    const tests = [
      {
        input: "if (true) { 10 }",
        expected: 10,
      },
      {
        input: "if (false) { 10 }",
        expected: null,
      },
      {
        input: "if (1) { 10 }",
        expected: 10,
      },
      {
        input: "if (1 < 2) { 10 }",
        expected: 10,
      },
      {
        input: "if (1 > 2) { 10 }",
        expected: null,
      },
      {
        input: "if (1 > 2) { 10 } else { 20 }",
        expected: 20,
      },
      {
        input: "if (1 < 2) { 10 } else { 20 }",
        expected: 10,
      },
    ];

    tests.forEach((tt) => {
      const evaluated = makeSut(tt.input);
      expect(evaluated?.value).toEqual(tt.expected);
    });
  });
});

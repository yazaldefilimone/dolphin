import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";

describe("Parser", () => {
  describe("parse program", () => {
    it("operator precedence parsing", () => {
      const tests = [
        { input: "-a * b", expected: "((-a) * b)" },
        { input: "!-a", expected: "(!(-a))" },
        { input: "a + b + c", expected: "((a + b) + c)" },
        { input: "a + b - c", expected: "((a + b) - c)" },
        { input: "a * b * c", expected: "((a * b) * c)" },
        { input: "a * b / c", expected: "((a * b) / c)" },
        { input: "a + b / c", expected: "(a + (b / c))" },
        {
          input: "a + b * c + d / e - f",
          expected: "(((a + (b * c)) + (d / e)) - f)",
        },
        { input: "3 + 4; -5 * 5", expected: "(3 + 4)((-5) * 5)" },
        { input: "5 > 4 == 3 < 4", expected: "((5 > 4) == (3 < 4))" },
        { input: "5 < 4 != 3 > 4", expected: "((5 < 4) != (3 > 4))" },
        {
          input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
          expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
      ];
      tests.forEach((tt) => {
        const { program } = makeSut(tt.input, {
          toString: true,
          isLogError: false,
        });
        expect(program.toString()).toBe(tt.expected);
      });
    });
  });
});

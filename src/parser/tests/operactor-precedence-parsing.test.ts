import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { ExpressionStatement, PrefixExpression } from "ast";

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
        const { program } = makeSut(tt.input);
        expect(program.toString()).toBe(tt.expected);
      });
    });
  });
  it("boolean and operator precedence parsing", () => {
    const tests = [
      { input: "true;", expected: "true" },
      { input: "false;", expected: "false" },
      { input: "3 > 5 == false", expected: "((3 > 5) == false)" },
      { input: "3 < 5 == true", expected: "((3 < 5) == true)" },
    ];

    tests.forEach((tt) => {
      const { program } = makeSut(tt.input);
      expect(program.toString()).toBe(tt.expected);
    });
  });
  it("boolean bang operator precedence parsing", () => {
    const tests = [
      { input: "!true", operator: "!", value: true },
      { input: "!false", operator: "!", value: false },
    ];
    tests.forEach((tt) => {
      const { program } = makeSut(tt.input);
      const statement: any = program.statements[0];
      expect(statement?.expression?.operator).toBe(tt.operator);
      expect(statement?.expression?.right.value).toBe(tt.value);
    });
  });

  it("operator group precedence parsing", () => {
    const tests = [
      { input: "1 + (2 + 3) + 4", expected: "((1 + (2 + 3)) + 4)" },
      { input: "(5 + 5) * 2", expected: "((5 + 5) * 2)" },
      { input: "2 / (5 + 5)", expected: "(2 / (5 + 5))" },
      { input: "-(5 + 5)", expected: "(-(5 + 5))" },
      { input: "!(true == true)", expected: "(!(true == true))" },
    ];
    tests.forEach((tt) => {
      const { program } = makeSut(tt.input, {
        isLogError: true,
        toString: false,
      });
      expect(program.toString()).toBe(tt.expected);
    });
  });
});

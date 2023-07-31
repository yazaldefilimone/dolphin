import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { ExpressionStatement } from "ast";
import { CallExpression } from "ast";

describe("Parser", () => {
  describe("parseProgram", () => {
    it("should parse call expression", () => {
      const code = "add(1, 2*3, 4+5);";
      const { program } = makeSut(code, {
        isLogError: true,
        toString: false,
      });

      expect(program).not.toBeNull();
      expect(program.statements).toHaveLength(1);
      expect(program.statements[0]).toBeInstanceOf(ExpressionStatement);
      const expressionStatement = program.statements[0] as ExpressionStatement;
      expect(expressionStatement.expression).not.toBeNull();
      expect(expressionStatement.expression).toBeInstanceOf(CallExpression);
      const callExpression = expressionStatement.expression as any;
      expect(callExpression.toString()).toBe("add(1,(2 * 3),(4 + 5))");
      expect(callExpression.arguments).toHaveLength(3);
      expect(callExpression.arguments[0]).not.toBeNull();
      expect(callExpression.arguments[1]).not.toBeNull();
      expect(callExpression.arguments[2]).not.toBeNull();
      expect(callExpression.arguments[0].toString()).toBe("1");
      expect(callExpression.arguments[1].toString()).toBe("(2 * 3)");
      expect(callExpression.arguments[2].toString()).toBe("(4 + 5)");
    });
  });
  it("should operator precedence parser", () => {
    const tests = [
      {
        code: `a + add(b * c) + b`,
        expect: `((a + add((b * c))) + b)`,
      },
      {
        code: `add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))`,
        expect: `add(a,b,1,(2 * 3),(4 + 5),add(6,(7 * 8)))`,
      },
      {
        code: `add(a + b + c * d / f + g)`,
        expect: `add((((a + b) + ((c * d) / f)) + g))`,
      },
    ];
    tests.forEach((test) => {
      const { program } = makeSut(test.code, {
        isLogError: true,
        toString: false,
      });
      expect(program.toString()).toEqual(test.expect);
    });
  });
});

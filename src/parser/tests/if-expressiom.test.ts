import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { ExpressionStatement, IfExpression } from "ast";

describe("Parser", () => {
  describe("parseProgram", () => {
    it("should parse if expression", () => {
      const input = `if (x < y) { x }`;
      const { program, errors } = makeSut(input, {
        isLogError: true,
        toString: false,
      });
      expect(errors.length).toBe(0);
      expect(program.statements.length).toBe(1);
      const stmt = program.statements[0] as ExpressionStatement;
      expect(stmt).toBeInstanceOf(ExpressionStatement);
      const exp = stmt.expression;
      expect(exp).toBeInstanceOf(IfExpression);
      const ifExp = exp as IfExpression;
      expect(ifExp?.condition?.toString()).toBe("(x < y)");
      expect(ifExp?.consequence?.toString()).toBe("{x}");
      expect(ifExp.alternative).toBeNull();
      expect(exp?.toString()).toEqual("if (x < y) {x}");
    });
    it("should parse if else expression", () => {
      const input = `if (x < y) { x } else { y }`;
      const { program, errors } = makeSut(input, {
        isLogError: true,
        toString: false,
      });
      expect(errors.length).toBe(0);
      expect(program.statements.length).toBe(1);
      const stmt = program.statements[0] as ExpressionStatement;
      expect(stmt).toBeInstanceOf(ExpressionStatement);
      const exp = stmt.expression;
      expect(exp).toBeInstanceOf(IfExpression);
      const ifExp = exp as IfExpression;
      expect(ifExp.condition?.toString()).toBe("(x < y)");
      expect(ifExp.consequence?.toString()).toBe("{x}");
      expect(ifExp.alternative?.toString()).toBe("{y}");
      expect(stmt.toString()).toEqual(`if (x < y) {x} else {y}`);
    });
  });
});

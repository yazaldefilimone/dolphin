import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { ExpressionStatement, StringLiteral } from "ast";

describe("Parser", () => {
  describe("String", () => {
    it("should parse string", () => {
      const input = `"hello world";`;
      const expected = "hello world";
      const { program } = makeSut(input);
      expect(program.statements.length).toBe(1);
      const stmt = program.statements[0] as unknown as ExpressionStatement;
      expect(stmt).toBeDefined();
      const expression = stmt.expression as StringLiteral;
      expect(expression).toBeDefined();
      expect(expression).toBeInstanceOf(StringLiteral);
      expect(expression.tokenLiteral()).toBe(expected);
      expect(expression.value).toBe(expected);
    });
  });
});

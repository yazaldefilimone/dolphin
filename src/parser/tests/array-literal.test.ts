import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { ExpressionStatement } from "ast";
import { ArrayLiteral } from "ast/array";
describe("Parser", () => {
  describe("Parse program", () => {
    it("Array literal", () => {
      const code = `
      [1, 2 * 2, 3 + 3, "hello"];
    `;
      const { program } = makeSut(code, {
        isLogError: true,
        toString: false,
      });
      const statement: ExpressionStatement = program.statements[0] as any;
      expect(statement).instanceOf(ExpressionStatement);
      const expression = statement.expression as any;
      expect(expression).instanceOf(ArrayLiteral);
      expect(expression.elements.length).toBe(4);
      expect(expression.elements[0].value).toBe(1);
      expect(expression.elements[1].toString()).toBe("(2 * 2)");
      expect(expression.elements[2].toString()).toBe("(3 + 3)");
      expect(expression.elements[3].value).toBe("hello");
    });
  });
});

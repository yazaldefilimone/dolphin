import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { BooleanLiteral, ExpressionStatement, LetStatement } from "ast";
import { IndexExpression } from "ast/index-expression";
describe("Parser", () => {
  describe("Parse program", () => {
    it("index expression", () => {
      const code = `
      myArray[1 + 1];
    `;
      const { program } = makeSut(code, {
        isLogError: true,
        toString: true,
      });
      const statement: ExpressionStatement = program.statements[0] as any;
      const expression = statement.expression as IndexExpression;
      expect(expression).instanceOf(IndexExpression);
      expect(expression.left?.toString()).toBe("myArray");
      expect(expression.index?.toString()).toBe("(1 + 1)");
    });
  });
});

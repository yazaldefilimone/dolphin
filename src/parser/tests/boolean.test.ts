import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { BooleanLiteral, ExpressionStatement, LetStatement } from "ast";
describe("Parser", () => {
  describe("Parse program", () => {
    it("boolean literal", () => {
      const code = `
    true;
    false;
    `;
      const tests = [
        { input: "true;", expected: "true", value: true },
        { input: "false;", expected: "false", value: false },
      ];
      const { program } = makeSut(code, {
        isLogError: true,
        toString: false,
      });
      tests.forEach((tt, index) => {
        const statement: ExpressionStatement = program.statements[index] as any;
        const expression = statement.expression as BooleanLiteral;
        expect(expression).instanceOf(BooleanLiteral);
        expect(expression.tokenLiteral()).toBe(tt.expected);
        expect(expression.value).toBe(tt.value);
        // TODO: fix this
        // const expression = statement.value;
        // expect(expression).toBeInstanceOf(BooleanLiteral);
        // expect(expression.tokenLiteral()).toEqual(tt.expected);
      });
    });
  });
});

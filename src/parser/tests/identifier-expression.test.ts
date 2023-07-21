import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { ExpressionStatement, Identifier } from "ast";
describe("Parser", () => {
  describe("Parse program", () => {
    it("identifier expression", () => {
      const code = "foobar;";
      const { program } = makeSut(code);
      expect(program.statements.length === 1).toBeTruthy();
      const statement: any = program.statements[0];
      expect(statement).toBeInstanceOf(ExpressionStatement);
      const expression = statement.expression;
      expect(expression).toBeInstanceOf(Identifier);
      expect(expression.tokenLiteral()).toEqual("foobar");
      expect(statement.tokenLiteral()).toEqual("foobar");
    });
  });
});

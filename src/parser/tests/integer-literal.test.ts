import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { ExpressionStatement, IntegerLiteral } from "ast";

describe("Parser", () => {
  describe("Parse program", () => {
    it("integer literal expression", () => {
      const code = "5;";
      const { program } = makeSut(code);
      expect(program.statements.length === 1).toBeTruthy();
      const statement: any = program.statements[0];
      expect(statement).toBeInstanceOf(ExpressionStatement);
      const expression = statement.expression;
      expect(expression).toBeInstanceOf(IntegerLiteral);
      expect(expression.tokenLiteral()).toEqual("5");
    });
  });
});

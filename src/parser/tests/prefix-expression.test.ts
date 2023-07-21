import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import {
  ExpressionStatement,
  PrefixExpression,
  IntegerLiteral,
  Identifier,
} from "ast";
import { TokenType } from "token";

describe("Parser", () => {
  describe("Parse program", () => {
    it("prefix expression", () => {
      const code = `
      -5;
      !foobar;
      `;
      const { program } = makeSut(code);
      expect(program.statements.length === 2).toBeTruthy();
      const statement: any = program.statements[0];
      expect(statement).toBeInstanceOf(ExpressionStatement);
      const expression = statement.expression;
      expect(expression).toBeInstanceOf(PrefixExpression);
      expect(expression.tokenLiteral()).toEqual("-");
      expect(expression.operator).toEqual("-");
      expect(expression.right).toBeInstanceOf(IntegerLiteral);
      expect(expression.right.tokenLiteral()).toEqual("5");
      const statement2: any = program.statements[1];
      expect(statement2).toBeInstanceOf(ExpressionStatement);
      const expression2 = statement2.expression;
      expect(expression2).toBeInstanceOf(PrefixExpression);
      expect(expression2.tokenLiteral()).toEqual("!");
      expect(expression2.operator).toEqual("!");
      expect(expression2.right).toBeInstanceOf(Identifier);
      expect(expression2.right.tokenLiteral()).toEqual("foobar");
    });
  });
});

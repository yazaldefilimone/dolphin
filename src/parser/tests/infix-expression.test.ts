import { ExpressionStatement, InfixExpression, IntegerLiteral } from "ast";
import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";

describe("Parser", () => {
  describe("parse program", () => {
    it("infix expression", () => {
      const infixTests = [
        {
          input: "5 + 5;",
          leftValue: 5,
          operator: "+",
          rightValue: 5,
        },
        {
          input: "5 - 5;",
          leftValue: 5,
          operator: "-",
          rightValue: 5,
        },
        {
          input: "5 * 5;",
          leftValue: 5,
          operator: "*",
          rightValue: 5,
        },
        {
          input: "5 / 5;",
          leftValue: 5,
          operator: "/",
          rightValue: 5,
        },
        {
          input: "5 > 5;",
          leftValue: 5,
          operator: ">",
          rightValue: 5,
        },
        {
          input: "5 < 5;",
          leftValue: 5,
          operator: "<",
          rightValue: 5,
        },
        {
          input: "5 == 5;",
          leftValue: 5,
          operator: "==",
          rightValue: 5,
        },
        {
          input: "5 != 5;",
          leftValue: 5,
          operator: "!=",
          rightValue: 5,
        },
        {
          input: "true == true;",
          leftValue: true,
          operator: "==",
          rightValue: true,
        },
        {
          input: "true != false;",
          leftValue: true,
          operator: "!=",
          rightValue: false,
        },
        {
          input: "false == false;",
          leftValue: false,
          operator: "==",
          rightValue: false,
        },
      ];
      infixTests.forEach((infixTest) => {
        const { program } = makeSut(infixTest.input);
        expect(program.statements.length === 1).toBeTruthy();
        const statement: any = program.statements[0];
        expect(statement).toBeInstanceOf(ExpressionStatement);
        const expression = statement.expression;
        expect(expression).toBeInstanceOf(InfixExpression);
        expect(expression.tokenLiteral()).toEqual(infixTest.operator);
        expect(expression.operator).toEqual(infixTest.operator);
        expect(expression.left.value).toEqual(infixTest.leftValue);
        expect(expression.right.value).toEqual(infixTest.rightValue);
      });
    });
  });
});

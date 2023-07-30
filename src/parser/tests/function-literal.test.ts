import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { FunctionLiteral } from "ast/function";
import { BlockStatement, ExpressionStatement } from "ast";

describe("Parser", () => {
  describe("parseProgram", () => {
    it("parse function literal", () => {
      let code = "fn(x, y) { x + y; }";
      const { program } = makeSut(code, {
        isLogError: true,
        toString: false,
      });
      expect(program.statements.length).toBe(1);
      const statement = program.statements[0] as ExpressionStatement;
      expect(statement.tokenLiteral()).toBe("fn");
      expect(statement).toBeInstanceOf(ExpressionStatement);
      expect(statement.expression).toBeInstanceOf(FunctionLiteral);
      const functionLiteral = statement.expression as FunctionLiteral;
      expect(functionLiteral.parameters?.length).toBe(2);
      expect(functionLiteral.parameters?.[0].toString()).toBe("x");
      expect(functionLiteral.parameters?.[1].toString()).toBe("y");
      expect(functionLiteral.body).toBeInstanceOf(BlockStatement);
      const body = functionLiteral.body as BlockStatement;
      expect(body?.statements.length).toBe(1);
      expect(body?.statements[0]).toBeInstanceOf(ExpressionStatement);
      const bodyStatement = body?.statements[0] as ExpressionStatement;
      expect(bodyStatement?.toString()).toBe("(x + y)");
      expect(statement.toString()).toBe("fn(x,y) {(x + y)}");
    });
    it("parse function literal with no parameters", () => {
      let code = "fn() { x + y; }";
      const { program } = makeSut(code, {
        isLogError: true,
        toString: false,
      });
      expect(program.statements.length).toBe(1);
      const statement = program.statements[0] as ExpressionStatement;
      expect(statement.tokenLiteral()).toBe("fn");
      expect(statement).toBeInstanceOf(ExpressionStatement);
      expect(statement.expression).toBeInstanceOf(FunctionLiteral);
      const functionLiteral = statement.expression as FunctionLiteral;
      expect(functionLiteral.parameters?.length).toBe(0);
      expect(functionLiteral.body).toBeInstanceOf(BlockStatement);
      const body = functionLiteral.body as BlockStatement;
      expect(body?.statements.length).toBe(1);
      expect(body?.statements[0]).toBeInstanceOf(ExpressionStatement);
      const bodyStatement = body?.statements[0] as ExpressionStatement;
      expect(bodyStatement?.toString()).toBe("(x + y)");
      expect(statement.toString()).toBe("fn() {(x + y)}");
    });
    it("parse function literal with no parameters and body", () => {
      let code = "fn() {}";
      const { program } = makeSut(code, {
        isLogError: true,
        toString: false,
      });
      expect(program.statements.length).toBe(1);
      const statement = program.statements[0] as ExpressionStatement;
      expect(statement.tokenLiteral()).toBe("fn");
      expect(statement).toBeInstanceOf(ExpressionStatement);
      expect(statement.expression).toBeInstanceOf(FunctionLiteral);
      const functionLiteral = statement.expression as FunctionLiteral;
      expect(functionLiteral.parameters?.length).toBe(0);
      expect(functionLiteral.body).toBeInstanceOf(BlockStatement);
      const body = functionLiteral.body as BlockStatement;
      expect(body?.statements.length).toBe(0);
      expect(statement.toString()).toBe("fn() {}");
    });
    it("parse function literal with parameters and no body", () => {
      let code = "fn(x, y) {}";
      const { program } = makeSut(code, {
        isLogError: true,
        toString: false,
      });
      expect(program.statements.length).toBe(1);
      const statement = program.statements[0] as ExpressionStatement;
      expect(statement.tokenLiteral()).toBe("fn");
      expect(statement).toBeInstanceOf(ExpressionStatement);
      expect(statement.expression).toBeInstanceOf(FunctionLiteral);
      const functionLiteral = statement.expression as FunctionLiteral;
      expect(functionLiteral.parameters?.length).toBe(2);
      expect(functionLiteral.parameters?.[0].toString()).toBe("x");
      expect(functionLiteral.parameters?.[1].toString()).toBe("y");
      expect(functionLiteral.body).toBeInstanceOf(BlockStatement);
      const body = functionLiteral.body as BlockStatement;
      expect(body?.statements.length).toBe(0);
      expect(statement.toString()).toBe("fn(x,y) {}");
    });
  });
});

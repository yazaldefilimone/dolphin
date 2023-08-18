import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { TokenType } from "token";

describe("Parser", () => {
  describe("Parse program", () => {
    it("return statement", () => {
      const code = `
    return 5;
    return 10;
    return 993322;
    `;
      const tests = [
        { expectedValue: 5 },
        { expectedValue: 10 },
        { expectedValue: 993322 },
      ];
      const { program, errors } = makeSut(code);
      expect(program.statements.length).toBe(3);
      expect(errors.length).toBe(0);
      tests.forEach((test, index) => {
        const statement: any = program.statements[index];
        expect(statement.token.type).toEqual(TokenType.RETURN);
        expect(statement.tokenLiteral()).toEqual("return");
        expect(statement.returnValue?.value).toEqual(test.expectedValue);
      });
    });
  });
});

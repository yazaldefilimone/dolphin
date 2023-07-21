import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { Program } from "ast";
import { TokenType } from "token";

describe("Parser", () => {
  describe("Parse program", () => {
    it("should parse let statement", () => {
      const code = `
    let x = 5;
    let y = 10;
    let foobar = 838383;
    `;
      const { program } = makeSut(code);
      const tests = [
        { expectedIdentifier: "x" },
        { expectedIdentifier: "y" },
        { expectedIdentifier: "foobar" },
      ];
      expect(program).not.toBeNull();
      expect(program).instanceOf(Program);
      expect(program.statements.length).toBe(3);
      tests.forEach((test, index) => {
        const statement: any = program.statements[index];
        let identifier = statement.name;
        expect(statement.tokenLiteral()).toBe("let");
        expect(identifier.tokenLiteral()).toBe(test.expectedIdentifier);
        expect(identifier.token.type).toBe(TokenType.IDENT);
      });
    });
  });
});

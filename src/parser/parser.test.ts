import { Lexer } from "lexer";
import { it, expect, describe } from "vitest";
import { Parser } from "parser";
import { Program } from "ast";
import { TokenType } from "token";

describe("parser", () => {
  it("test let statement", () => {
    const code = `
    let x = 5;
    let y = 10;
    let foobar = 838383;
    `;
    const lexer = new Lexer(code);
    const parser = new Parser(lexer);
    const tests = [
      { expectedIdentifier: "x" },
      { expectedIdentifier: "y" },
      { expectedIdentifier: "foobar" },
    ];
    const program = parser.parseProgram();
    expect(program).not.toBeNull();
    expect(program).instanceOf(Program);
    expect(program.statements.length).toBe(3);
    console.log(JSON.stringify(program.statements, null, 2));
    tests.forEach((test, index) => {
      const statement: any = program.statements[index];
      let identifier = statement.name;
      expect(statement).not.toBeNull();
      expect(statement.tokenLiteral()).toBe("let");
      expect(identifier.tokenLiteral()).toBe(test.expectedIdentifier);
      expect(identifier.token.type).toBe(TokenType.IDENT);
    });
  });
});

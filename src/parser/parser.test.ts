import { Lexer } from "lexer";
import { it, expect, describe } from "vitest";
import { ErrorHandler, Parser } from "parser";
import { Program } from "ast";
import { TokenType } from "token";

function printError(errors: ErrorHandler) {
  errors.getErrorsInString().map((message) => {
    console.log({ error: message });
  });
}
const makeSut = (code: string) => {
  const lexer = new Lexer(code);
  const parser = new Parser(lexer);
  const program = parser.parseProgram();
  const errors = parser.errorHandler.getErrors();
  const errorsInString = parser.errorHandler.getErrorsInString();
  const errorHandler = parser.errorHandler;
  console.log(program.toString());
  printError(parser.errorHandler);
  return { lexer, parser, program, errors, errorsInString, errorHandler };
};
describe("parser", () => {
  it("test let statement", () => {
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
  it("test error handler", () => {
    const code = `
    let x 5;
    let = 10;
    let 838383;
    `;
    const { program, parser, errors } = makeSut(code);
    expect(program).instanceOf(Program);
    expect(parser.errorHandler).instanceOf(ErrorHandler);
    printError(parser.errorHandler);
    expect(errors.length).toBe(3);
  });

  it("test return statement", () => {
    const code = `
    return 5;
    return 10;
    return 993322;
    `;
    let tests = [
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
      // expect(statement.returnValue).toEqual(test.expectedValue);
    });
  });
});

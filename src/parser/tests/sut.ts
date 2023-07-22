import { Program } from "ast";
import { Lexer } from "lexer";
import { ErrorHandler, Parser } from "parser";
function debugPrintError(errors: ErrorHandler) {
  errors.getErrors().map((error) => {
    console.log({ error });
  });
}

function debugPrintErrorInString(errors: ErrorHandler) {
  errors.getErrorsInString().map((message) => {
    const error = "Error: " + message.concat("\n");
    console.log("\x1b[31m%s\x1b[0m", error);
  });
}
function debugToString(program: Program) {
  console.log(program.toString().concat("\n"));
}
export const makeSut = (code: string, isLogError = false, toString = false) => {
  const lexer = new Lexer(code);
  const parser = new Parser(lexer);
  const program = parser.parseProgram();
  const errors = parser.errorHandler.getErrors();
  const errorsInString = parser.errorHandler.getErrorsInString();
  const errorHandler = parser.errorHandler;
  toString && debugToString(program);
  isLogError && debugPrintErrorInString(parser.errorHandler);
  return { lexer, parser, program, errors, errorsInString, errorHandler };
};

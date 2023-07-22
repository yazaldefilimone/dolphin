import { Lexer } from "lexer";
import { ErrorHandler, Parser } from "parser";
function printError(errors: ErrorHandler) {
  errors.getErrorsInString().map((message) => {
    console.log({ error: message });
  });
}
export const makeSut = (code: string, isLogError = false, toString = false) => {
  const lexer = new Lexer(code);
  const parser = new Parser(lexer);
  const program = parser.parseProgram();
  const errors = parser.errorHandler.getErrors();
  const errorsInString = parser.errorHandler.getErrorsInString();
  const errorHandler = parser.errorHandler;
  toString && console.log(program.toString());
  isLogError && printError(parser.errorHandler);
  return { lexer, parser, program, errors, errorsInString, errorHandler };
};

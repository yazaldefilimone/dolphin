import { Evaluator } from "evaluator";
import { Lexer } from "lexer";
import { Parser } from "parser";

export function makeSut(input: string) {
  const lexer = new Lexer(input);
  const parser = new Parser(lexer);
  const program = parser.parseProgram();
  const evaluator = Evaluator(program);
  return evaluator;
}

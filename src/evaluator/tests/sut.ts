import { Evaluator } from "evaluator";
import { BaseObject } from "evaluator/object";
import { Lexer } from "lexer";
import { Parser } from "parser";
import { Maybe } from "utils";

export function makeSut<T = unknown>(input: string) {
  const lexer = new Lexer(input);
  const parser = new Parser(lexer);
  const program = parser.parseProgram();
  const evaluator = Evaluator(program);
  return evaluator as unknown as Maybe<BaseObject<T>>;
}

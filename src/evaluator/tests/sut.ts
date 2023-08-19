import { Evaluator } from "evaluator";
import { BaseObject, Environment } from "evaluator/object";
import { Lexer } from "lexer";
import { Parser } from "parser";
import { Maybe } from "utils";

export function makeSut<T = unknown>(input: string) {
  const lexer = new Lexer(input);
  const parser = new Parser(lexer);
  const program = parser.parseProgram();
  const env = new Environment();
  const evaluator = Evaluator(program, env);
  return evaluator as unknown as Maybe<BaseObject<T>>;
}

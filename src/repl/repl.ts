import readline from "node:readline";
import { stdin, stdout } from "node:process";

import { Lexer } from "lexer";
import { ErrorHandler, Parser } from "parser";
import { Evaluator } from "evaluator";

const ScannerClose = {
  exit: "exit",
  quit: "quit",
};

const exits = [ScannerClose.exit, ScannerClose.quit];

export function startReadEvalPrintLoop() {
  const scanner = readline.createInterface({
    input: stdin,
    output: stdout,
  });

  function repl() {
    scanner.question(">> ", (input) => {
      if (exits.includes(input)) return scanner.close();
      const lexer = new Lexer(input);
      const parser = new Parser(lexer);
      const program = parser.parseProgram();
      if (parser.isError()) {
        printError(parser.errorHandler);
        repl();
      }
      const evaluate = Evaluator(program);
      if (evaluate) console.log(evaluate.inspect(), "\n");
      repl();
    });
  }
  console.log("Welcome!");
  console.log("The Dolphin Programming Language!");

  repl();
}

function printError(errorHandler: ErrorHandler) {
  console.log("Woops! We ran into some dolphin business here!");
  console.log(" parser errors:");
  errorHandler.getErrorsInString().forEach((error) => console.log(`\t${error}`, "\n"));
}

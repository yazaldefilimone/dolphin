import readline from "readline";
import { Lexer } from "lexer";
import { TokenType } from "token";

const ScannerClose = {
  exit: "exit",
  quit: "quit",
};

const exits = [ScannerClose.exit, ScannerClose.quit];

export function startReadEvalPrintLoop() {
  const scanner = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  function repl() {
    scanner.question(">> ", (input) => {
      if (exits.includes(input)) return scanner.close();
      const lexer = new Lexer(input);
      let token = lexer.nextToken();
      while (token.type !== TokenType.EOF) {
        console.log(token);
        token = lexer.nextToken();
      }
      repl();
    });
  }
  console.log("Welcome!");
  repl();
}

import { Token, TokenType } from "token";
import { it, expect, describe } from "vitest";
import { Lexer } from "lexer";

describe("Lexer", () => {
  it("should be return current token of symbols", () => {
    let input = "=+(){},;";
    let expects: Token[] = [
      new Token(TokenType.ASSIGN, "="),
      new Token(TokenType.PLUS, "+"),
      new Token(TokenType.LPAREN, "("),
      new Token(TokenType.RPAREN, ")"),
      new Token(TokenType.LBRACE, "{"),
      new Token(TokenType.RBRACE, "}"),
      new Token(TokenType.COMMA, ","),
      new Token(TokenType.SEMICOLON, ";"),
      new Token(TokenType.EOF, ""),
    ];
    let l = new Lexer(input);
    expects.forEach((element) => {
      let token = l.nextToken();
      if (token) {
        expect(token).instanceof(Token);
        expect(token.type).toEqual(element.type);
        expect(token.literal).toEqual(element.literal);
      } else if (element) {
        throw new Error("void");
      }
    });
  });
});

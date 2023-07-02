import { Token, TokenType } from "token";
import { it, expect, describe } from "vitest";
import { Lexer } from "lexer";

describe("Lexer", () => {
  it("should be return current token of symbols", () => {
    let code = `
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
      x + y;
    };
    let result = add(five, ten);
`;
    let tests = [
      new Token(TokenType.LET, "let"),
      new Token(TokenType.IDENT, "five"),
      new Token(TokenType.ASSIGN, "="),
      new Token(TokenType.INT, "5"),
      new Token(TokenType.SEMICOLON, ";"),
      new Token(TokenType.LET, "let"),
      new Token(TokenType.IDENT, "ten"),
      new Token(TokenType.ASSIGN, "="),
      new Token(TokenType.INT, "10"),
      new Token(TokenType.SEMICOLON, ";"),
      new Token(TokenType.LET, "let"),
      new Token(TokenType.IDENT, "add"),
      new Token(TokenType.ASSIGN, "="),
      new Token(TokenType.FUNCTION, "fn"),
      new Token(TokenType.LPAREN, "("),
      new Token(TokenType.IDENT, "x"),
      new Token(TokenType.COMMA, ","),
      new Token(TokenType.IDENT, "y"),
      new Token(TokenType.RPAREN, ")"),
      new Token(TokenType.LBRACE, "{"),
      new Token(TokenType.IDENT, "x"),
      new Token(TokenType.PLUS, "+"),
      new Token(TokenType.IDENT, "y"),
      new Token(TokenType.SEMICOLON, ";"),
      new Token(TokenType.RBRACE, "}"),
      new Token(TokenType.SEMICOLON, ";"),
      new Token(TokenType.LET, "let"),
      new Token(TokenType.IDENT, "result"),
      new Token(TokenType.ASSIGN, "="),
      new Token(TokenType.IDENT, "add"),
      new Token(TokenType.LPAREN, "("),
      new Token(TokenType.IDENT, "five"),
      new Token(TokenType.COMMA, ","),
      new Token(TokenType.IDENT, "ten"),
      new Token(TokenType.RPAREN, ")"),
      new Token(TokenType.SEMICOLON, ";"),
      new Token(TokenType.EOF, ""),
    ];

    let lexer = new Lexer(code);
    tests.forEach((element) => {
      let token = lexer.nextToken();
      expect(token).instanceof(Token);
      expect(token.type).toEqual(element.type);
      expect(token.literal).toEqual(element.literal);
    });
  });
});

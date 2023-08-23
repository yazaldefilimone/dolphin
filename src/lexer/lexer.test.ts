import { Token, TokenType } from "token";
import { it, expect, describe } from "vitest";
import { Lexer } from "lexer";

describe("Lexer", () => {
  it("should be return current token of symbols", () => {
    const code = `
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
      x + y;
    };
    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;
    if (5 < 10) {
      return true;
    } else {
      return false;
    }
    10 == 10;
    10 != 9;
    "foobar"
    "foo bar"
`;
    const tests = [
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
      new Token(TokenType.BANG, "!"),
      new Token(TokenType.MINUS, "-"),
      new Token(TokenType.SLASH, "/"),
      new Token(TokenType.ASTERISK, "*"),
      new Token(TokenType.INT, "5"),
      new Token(TokenType.SEMICOLON, ";"),
      new Token(TokenType.INT, "5"),
      new Token(TokenType.LT, "<"),
      new Token(TokenType.INT, "10"),
      new Token(TokenType.GT, ">"),
      new Token(TokenType.INT, "5"),
      new Token(TokenType.SEMICOLON, ";"),
      new Token(TokenType.IF, "if"),
      new Token(TokenType.LPAREN, "("),
      new Token(TokenType.INT, "5"),
      new Token(TokenType.LT, "<"),
      new Token(TokenType.INT, "10"),
      new Token(TokenType.RPAREN, ")"),
      new Token(TokenType.LBRACE, "{"),
      new Token(TokenType.RETURN, "return"),
      new Token(TokenType.TRUE, "true"),
      new Token(TokenType.SEMICOLON, ";"),
      new Token(TokenType.RBRACE, "}"),
      new Token(TokenType.ELSE, "else"),
      new Token(TokenType.LBRACE, "{"),
      new Token(TokenType.RETURN, "return"),
      new Token(TokenType.FALSE, "false"),
      new Token(TokenType.SEMICOLON, ";"),
      new Token(TokenType.RBRACE, "}"),
      new Token(TokenType.INT, "10"),
      new Token(TokenType.EQ, "=="),
      new Token(TokenType.INT, "10"),
      new Token(TokenType.SEMICOLON, ";"),
      new Token(TokenType.INT, "10"),
      new Token(TokenType.NOT_EQ, "!="),
      new Token(TokenType.INT, "9"),
      new Token(TokenType.SEMICOLON, ";"),
      new Token(TokenType.STRING, "foobar"),
      new Token(TokenType.STRING, "foo bar"),
      new Token(TokenType.EOF, ""),
    ];

    const lexer = new Lexer(code);
    tests.forEach((element) => {
      const token = lexer.nextToken();
      expect(token).instanceof(Token);
      expect(token.type).toEqual(element.type);
      expect(token.literal).toEqual(element.literal);
    });
  });
});

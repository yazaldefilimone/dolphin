import { TokenType } from "token";

export enum Precedence {
  LOWEST = 1, //  because in javaScript, 0 is falsy
  EQUALS, // ==
  LESSGREATER, // > or <
  SUM, // +
  PRODUCT, // *
  PREFIX, // -X or !X
  CALL, // myFunction(X)
  INDEX, // array[index]
}

type precedenceMapType = Partial<Record<TokenType, Precedence>>;
export const precedences: precedenceMapType = {
  [TokenType.LPAREN]: Precedence.CALL,
  [TokenType.ASTERISK]: Precedence.PRODUCT,
  [TokenType.SLASH]: Precedence.PRODUCT,
  [TokenType.LT]: Precedence.LESSGREATER,
  [TokenType.GT]: Precedence.LESSGREATER,
  [TokenType.PLUS]: Precedence.SUM,
  [TokenType.MINUS]: Precedence.SUM,
  [TokenType.EQ]: Precedence.EQUALS,
  [TokenType.NOT_EQ]: Precedence.EQUALS,
  [TokenType.LBRACKET]: Precedence.INDEX,
} as const;

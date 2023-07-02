export class Token {
  type: TokenType;
  literal: string;
  constructor(type: TokenType, literal: string) {
    this.type = type;
    this.literal = literal;
  }
}
export enum TokenType {
  ILLEGAL = "ILLEGAL",
  EOF = "EOF",
  // Identifiers + literals
  IDENT = "IDENT", // add, foobar, x, y, ...
  INT = "INT", // 1343456
  // Operators
  ASSIGN = "=",
  PLUS = "+",
  // Delimiters
  COMMA = ",",
  SEMICOLON = ";",
  LPAREN = "(",
  RPAREN = ")",
  LBRACE = "{",
  RBRACE = "}",
  // Keywords
  FUNCTION = "FUNCTION",
  LET = "LET",
}

type keywordsType = {
  [key: string]: TokenType;
};
export const keywords: keywordsType = {
  fn: TokenType.FUNCTION,
  let: TokenType.LET,
};

export function lookupIdentifier(identifier: string): TokenType {
  let token = keywords[identifier];
  if (token) {
    return token;
  }
  return TokenType.IDENT;
}

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
  STRING = "STRING",
  // Operators
  ASSIGN = "=",
  PLUS = "+",
  MINUS = "-",
  BANG = "!",
  ASTERISK = "*",
  SLASH = "/",
  // Delimiters
  COMMA = ",",
  SEMICOLON = ";",
  LPAREN = "(",
  RPAREN = ")",
  LBRACE = "{",
  RBRACE = "}",
  LBRACKET = "[",
  RBRACKET = "]",
  // Keywords
  FUNCTION = "FUNCTION",
  LET = "LET",
  TRUE = "TRUE",
  FALSE = "FALSE",
  IF = "IF",
  ELSE = "ELSE",
  RETURN = "RETURN",

  LT = "<",
  GT = ">",
  EQ = "==",
  NOT_EQ = "!=",
}

type keywordsType = {
  [key: string]: TokenType;
};
export const keywords: keywordsType = {
  fn: TokenType.FUNCTION,
  let: TokenType.LET,
  false: TokenType.FALSE,
  true: TokenType.TRUE,
  if: TokenType.IF,
  else: TokenType.ELSE,
  return: TokenType.RETURN,
};

export function lookupIdentifier(identifier: string): TokenType {
  const token = keywords[identifier];
  if (token) {
    return token;
  }
  return TokenType.IDENT;
}

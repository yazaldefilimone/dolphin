import { TokenType, Token } from "token/token";

export class Lexer {
  private input: string;
  private position: number;
  private readPosition: number;
  private currentCharacter: string;

  constructor(input: string) {
    this.input = input;
    this.position = 0;
    this.readPosition = 0;
    this.currentCharacter = "";
    this.readCharacter();
  }

  readCharacter() {
    if (this.readPosition >= this.input.length) {
      this.currentCharacter = "";
    } else {
      this.currentCharacter = this.input[this.readPosition];
    }
    this.position = this.readPosition;
    this.readPosition += 1;
  }

  public nextToken(): Token | void {
    switch (this.currentCharacter) {
      case "=":
        return this.createToken(TokenType.ASSIGN, "=");
      case ";":
        return this.createToken(TokenType.SEMICOLON, ";");
      case "{":
        return this.createToken(TokenType.LBRACE, "{");
      case "}":
        return this.createToken(TokenType.RBRACE, "}");
      case "(":
        return this.createToken(TokenType.LPAREN, "(");
      case ")":
        return this.createToken(TokenType.RPAREN, ")");
      case ",":
        return this.createToken(TokenType.COMMA, ",");
      case "+":
        return this.createToken(TokenType.PLUS, "+");
      case "":
        return this.createToken(TokenType.EOF, "");
      default:
    }
  }

  private createToken(type: TokenType, literal: string) {
    this.readCharacter();
    const token = new Token(type, literal);
    return token;
  }
}

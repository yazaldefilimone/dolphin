import { TokenType, Token, lookupIdentifier } from "token/token";

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

  public nextToken(): Token {
    this.skipWhiteSpace();
    switch (this.currentCharacter) {
      case "=":
        return this.createToken(TokenType.ASSIGN, "=");
      case "+":
        return this.createToken(TokenType.PLUS, "+");
      case "-":
        return this.createToken(TokenType.MINUS, "-");
      case "!":
        return this.createToken(TokenType.BANG, "!");
      case "/":
        return this.createToken(TokenType.SLASH, "/");
      case "*":
        return this.createToken(TokenType.ASTERISK, "*");
      case "<":
        return this.createToken(TokenType.LT, "<");
      case ">":
        return this.createToken(TokenType.GT, ">");
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
        if (this.isLetter(this.currentCharacter)) {
          let identifier = this.readIdentifier();
          let type = lookupIdentifier(identifier);
          const token = new Token(type, identifier);
          return token;
        }

        if (this.isDigit(this.currentCharacter)) {
          let type = TokenType.INT;
          let identifier = this.readNumber();
          const token = new Token(type, identifier);
          return token;
        }
        return this.createToken(TokenType.ILLEGAL, this.currentCharacter);
    }
  }
  // method / functions
  private createToken(type: TokenType, literal: string) {
    this.readCharacter();
    const token = new Token(type, literal);
    return token;
  }

  // reads
  readCharacter() {
    if (this.readPosition >= this.input.length) {
      this.currentCharacter = "";
    } else {
      this.currentCharacter = this.input[this.readPosition];
    }
    this.position = this.readPosition;
    this.readPosition += 1;
  }

  private readIdentifier(): string {
    let currentPosition = this.position;
    while (this.isLetter(this.currentCharacter)) {
      this.readCharacter();
    }
    return this.input.substring(currentPosition, this.position);
  }

  private readNumber(): string {
    let currentPosition = this.position;
    while (this.isDigit(this.currentCharacter)) {
      this.readCharacter();
    }
    return this.input.substring(currentPosition, this.position);
  }
  // validators
  private isWhiteSpace = (char: string) => {
    return Boolean(char == " " || char == "\t" || char == "\n" || char == "\r");
  };

  private isDigit(character: string): boolean {
    return Boolean("0" <= character && character <= "9");
  }

  private isLetter(character: string): boolean {
    let isCharLoweCase = "a" <= character && character <= "z";
    let isCharUpperCase = "A" <= character && character <= "Z";
    return Boolean(isCharLoweCase || isCharUpperCase || character === "_");
  }

  // others
  private skipWhiteSpace() {
    while (this.isWhiteSpace(this.currentCharacter)) {
      this.readCharacter();
    }
  }
}

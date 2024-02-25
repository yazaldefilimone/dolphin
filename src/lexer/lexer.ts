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
        const peekedAssignCharacter = this.peekCharacter();
        const storeAssignAfterCharacter = this.currentCharacter;
        if (peekedAssignCharacter == "=") {
          this.readCharacter();
          const token = this.createToken(TokenType.EQ, storeAssignAfterCharacter + this.currentCharacter);
          return token;
        }
        return this.createToken(TokenType.ASSIGN, "=");
      case "+":
        return this.createToken(TokenType.PLUS, "+");
      case "-":
        return this.createToken(TokenType.MINUS, "-");
      case "!":
        const peekedBangCharacter = this.peekCharacter();
        const storeBangAfterCharacter = this.currentCharacter;
        if (peekedBangCharacter == "=") {
          this.readCharacter();
          const token = this.createToken(TokenType.NOT_EQ, storeBangAfterCharacter + this.currentCharacter);
          return token;
        }
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
      case "[":
        return this.createToken(TokenType.LBRACKET, "[");
      case "]":
        return this.createToken(TokenType.RBRACKET, "]");
      case ",":
        return this.createToken(TokenType.COMMA, ",");
      case "":
        return this.createToken(TokenType.EOF, "");
      case '"':
        const literal = this.readString();
        const token = this.createToken(TokenType.STRING, literal);
        return token;
      default:
        if (this.isLetter(this.currentCharacter)) {
          const identifier = this.readIdentifier();
          const type = lookupIdentifier(identifier);
          const token = new Token(type, identifier);
          return token;
        }

        if (this.isDigit(this.currentCharacter)) {
          const type = TokenType.INT;
          const identifier = this.readNumber();
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

  private peekCharacter(): string {
    if (this.readPosition >= this.input.length) {
      return "";
    } else {
      return this.input[this.readPosition];
    }
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
    const currentPosition = this.position;
    while (this.isLetter(this.currentCharacter)) {
      this.readCharacter();
    }
    return this.input.substring(currentPosition, this.position);
  }

  private readString(): string {
    const currentPosition = this.position + 1;
    do {
      this.readCharacter();
    } while (this.currentCharacter != '"' && this.currentCharacter != "");
    return this.input.substring(currentPosition, this.position);
  }
  private readNumber(): string {
    const currentPosition = this.position;
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
    const isCharLoweCase = "a" <= character && character <= "z";
    const isCharUpperCase = "A" <= character && character <= "Z";
    return Boolean(isCharLoweCase || isCharUpperCase || character === "_");
  }

  // others
  private skipWhiteSpace() {
    while (this.isWhiteSpace(this.currentCharacter)) {
      this.readCharacter();
    }
  }
}

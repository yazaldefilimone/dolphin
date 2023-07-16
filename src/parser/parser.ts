import { Identifier, LetStatement, parseResult } from "ast";
import { Program } from "ast/program";
import { Lexer } from "lexer";
import { Token, TokenType } from "token";

export class Parser {
  private lexer: Lexer;
  private currentToken: Token;
  private peekToken: Token;
  constructor(lexer: Lexer) {
    this.lexer = lexer;
    this.nextToken();
    this.nextToken();
  }

  nextToken() {
    this.currentToken = this.peekToken;
    this.peekToken = this.lexer.nextToken();
  }

  parseProgram(): Program {
    let program = new Program();
    while (this.currentToken.type != TokenType.EOF) {
      let statement = this.parseStatement();

      if (statement != null) {
        program.statements.push(statement);
      }
      this.nextToken();
    }

    return program;
  }
  parseStatement() {
    switch (this.currentToken.type) {
      case TokenType.LET:
        return this.parseLetStatement();
      default:
        return null;
    }
  }
  parseLetStatement(): parseResult<LetStatement> {
    const letToken = new LetStatement(this.currentToken);
    if (!this.expectPeek(TokenType.IDENT)) {
      return null;
    }
    let identifier = new Identifier(this.currentToken);
    letToken.name = identifier;
    if (!this.peekTokenIs(TokenType.ASSIGN)) {
      return null;
    }
    while (!this.currentTokenIs(TokenType.SEMICOLON)) {
      this.nextToken();
    }
    return letToken;
  }

  currentTokenIs(tokenType: TokenType) {
    return this.currentToken.type == tokenType;
  }
  peekTokenIs(tokenType: TokenType) {
    return this.peekToken.type == tokenType;
  }
  expectPeek(tokenType: TokenType) {
    if (this.peekTokenIs(tokenType)) {
      this.nextToken();
      return true;
    } else {
      return false;
    }
  }
}

import { Identifier, LetStatement, ReturnStatement, parseResult } from "ast";
import { Program } from "ast/program";
import { Lexer } from "lexer";
import { ErrorHandler } from "parser";
import { Token, TokenType } from "token";

export class Parser {
  private lexer: Lexer;
  private currentToken: Token;
  private peekToken: Token;
  public errorHandler: ErrorHandler;
  constructor(lexer: Lexer) {
    this.lexer = lexer;
    this.errorHandler = new ErrorHandler();
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
      case TokenType.RETURN:
        return this.parseReturnStatement();
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
    if (!this.expectPeek(TokenType.ASSIGN)) {
      return null;
    }
    while (!this.isCurrentToken(TokenType.SEMICOLON)) {
      this.nextToken();
    }
    return letToken;
  }
  parseReturnStatement(): parseResult<ReturnStatement> {
    const returnToken = new ReturnStatement(this.currentToken);
    this.nextToken();
    while (!this.isCurrentToken(TokenType.SEMICOLON)) {
      this.nextToken();
    }
    return returnToken;
  }

  isCurrentToken(tokenType: TokenType) {
    return this.currentToken.type == tokenType;
  }
  isPeekToken(tokenType: TokenType) {
    return this.peekToken.type == tokenType;
  }
  expectPeek(tokenType: TokenType) {
    if (this.isPeekToken(tokenType)) {
      this.nextToken();
      return true;
    } else {
      this.peekError(tokenType);
      return false;
    }
  }
  peekError(tokenType: TokenType) {
    const message = `expected next token to be ${tokenType}, got ${this.peekToken.type} instead`;
    this.errorHandler.push(message);
  }
}

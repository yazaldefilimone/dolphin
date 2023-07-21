import {
  Expression,
  Identifier,
  LetStatement,
  ReturnStatement,
  parseResult,
} from "ast";
import { ExpressionStatement } from "ast/expression";
import { Program } from "ast/program";
import { Lexer } from "lexer";
import { ErrorHandler, Precedence } from "parser";
import { Token, TokenType } from "token";
import { prefixParseFn, infixParseFn } from "parser";
import { IntegerLiteral } from "ast/integer-literal";
import { PrefixExpression } from "ast/prefix-expression";

export class Parser {
  private lexer: Lexer;
  private currentToken: Token;
  private peekToken: Token;
  public errorHandler: ErrorHandler;
  prefixParseFns: Map<TokenType, prefixParseFn>;
  infixParseFns: Map<TokenType, infixParseFn>;
  constructor(lexer: Lexer) {
    this.lexer = lexer;
    this.errorHandler = new ErrorHandler();
    this.nextToken();
    this.nextToken();
    this.prefixParseFns = new Map();
    // arrow function for prevent lose the context of `this`
    this.registerPrefix(TokenType.IDENT, () => this.parseIdentifier());
    this.registerPrefix(TokenType.INT, () => this.parseIntegerLiteral());
    this.registerPrefix(TokenType.BANG, () => this.parsePrefixExpression());
    this.registerPrefix(TokenType.MINUS, () => this.parsePrefixExpression());
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
  // ---- parse statement ----
  parseStatement() {
    switch (this.currentToken.type) {
      case TokenType.LET:
        return this.parseLetStatement();
      case TokenType.RETURN:
        return this.parseReturnStatement();
      default:
        return this.parseExpressionStatement();
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
  parseExpressionStatement(): parseResult<ExpressionStatement> {
    const expressionStatement = new ExpressionStatement(this.currentToken);
    const expression = this.parseExpression(Precedence.LOWEST);
    if (this.isPeekToken(TokenType.SEMICOLON)) {
      this.nextToken();
    }
    expressionStatement.expression = expression;
    return expressionStatement;
  }
  parseExpression(precedence: Precedence): parseResult<Expression> {
    const prefix = this.prefixParseFns.get(this.currentToken.type);
    if (prefix === undefined) {
      this.noPrefixParseFnError(this.currentToken.type);
      return null;
    }
    const leftExpression = prefix();
    return leftExpression;
  }
  parseIdentifier(): parseResult<Identifier> {
    const token = new Identifier(this.currentToken);
    token.value = this.currentToken.literal;
    return token;
  }
  parseIntegerLiteral(): parseResult<IntegerLiteral> {
    const integerLiteral = new IntegerLiteral(this.currentToken);
    const value = parseInt(this.currentToken.literal);
    if (isNaN(value)) {
      const message = `could not parse ${this.currentToken.literal} as integer`;
      this.pushError(message);
      return null;
    }
    integerLiteral.value = value;
    return integerLiteral;
  }
  parsePrefixExpression(): parseResult<PrefixExpression> {
    const expressionStatement = new PrefixExpression(
      this.currentToken,
      this.currentToken.literal
    );
    this.nextToken();
    expressionStatement.right = this.parseExpression(Precedence.PREFIX);
    return expressionStatement;
  }
  //  --- registers ---
  registerPrefix(tokenType: TokenType, fn: prefixParseFn) {
    this.prefixParseFns.set(tokenType, fn);
  }

  registerInfix(tokenType: TokenType, fn: infixParseFn) {
    this.infixParseFns.set(tokenType, fn);
  }

  // ---- validate token type ----
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

  // ---- error handling ----
  peekError(tokenType: TokenType) {
    const message = `expected next token to be "${tokenType}", got "${this.peekToken.type}" instead`;
    this.pushError(message);
  }

  noPrefixParseFnError(tokenType: TokenType) {
    const message = `no prefix parse function for "${tokenType}" found`;
    this.pushError(message);
  }
  pushError(message: string) {
    this.errorHandler.push(message);
  }
}

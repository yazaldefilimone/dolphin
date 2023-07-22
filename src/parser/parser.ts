import {
  Expression,
  Identifier,
  InfixExpression,
  LetStatement,
  ReturnStatement,
  parseResult,
} from "ast";
import { ExpressionStatement } from "ast/expression";
import { Program } from "ast/program";
import { Lexer } from "lexer";
import { ErrorHandler, Precedence, precedences } from "parser";
import { Token, TokenType } from "token";
import { prefixParseFn, infixParseFn } from "parser";
import { IntegerLiteral } from "ast/integer-literal";
import { PrefixExpression } from "ast";

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
    this.infixParseFns = new Map();
    // prefix registers
    this.registerPrefix(TokenType.IDENT, this.parseIdentifier.bind(this));
    this.registerPrefix(TokenType.INT, this.parseIntegerLiteral.bind(this));
    this.registerPrefix(TokenType.BANG, this.parsePrefixExpression.bind(this));
    this.registerPrefix(TokenType.MINUS, this.parsePrefixExpression.bind(this));
    // infix registers
    this.registerInfix(TokenType.PLUS, this.parseInfixExpression.bind(this));
    this.registerInfix(TokenType.LT, this.parseInfixExpression.bind(this));
    this.registerInfix(TokenType.GT, this.parseInfixExpression.bind(this));
    this.registerInfix(TokenType.MINUS, this.parseInfixExpression.bind(this));
    this.registerInfix(TokenType.SLASH, this.parseInfixExpression.bind(this));
    this.registerInfix(
      TokenType.ASTERISK,
      this.parseInfixExpression.bind(this)
    );
    this.registerInfix(TokenType.EQ, this.parseInfixExpression.bind(this));
    this.registerInfix(TokenType.NOT_EQ, this.parseInfixExpression.bind(this));
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
    let leftExpression = prefix();

    while (
      !this.isPeekToken(this.currentToken.type) &&
      precedence < this.peekPrecedence()
    ) {
      let infix = this.infixParseFns.get(this.peekToken.type);
      if (infix === undefined) {
        return leftExpression;
      }
      this.nextToken();
      if (leftExpression !== null) {
        leftExpression = infix(leftExpression);
      } else {
        leftExpression = null;
      }
    }
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
  parseInfixExpression(left: Expression): parseResult<Expression> {
    const operator = this.currentToken.literal;
    const token = this.currentToken;
    const expression = new InfixExpression(token, operator);
    expression.left = left;
    const precedence = this.currentPrecedence();
    this.nextToken();
    expression.right = this.parseExpression(precedence);
    return expression;
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
  peekPrecedence() {
    const precedence = precedences[this.peekToken.type];
    if (precedence) {
      return precedence;
    }
    return Precedence.LOWEST;
  }
  currentPrecedence() {
    const precedence = precedences[this.currentToken.type];
    if (precedence) {
      return precedence;
    }
    return Precedence.LOWEST;
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

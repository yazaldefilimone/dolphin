import {
  BlockStatement,
  BooleanLiteral,
  CallExpression,
  Expression,
  Identifier,
  IfExpression,
  InfixExpression,
  LetStatement,
  ReturnStatement,
  Statement,
  StringLiteral,
} from "ast";
import { Maybe } from "utils";

import { ExpressionStatement } from "ast/expression";
import { Program } from "ast/program";
import { Lexer } from "lexer";
import { ErrorHandler, Precedence, precedences } from "parser";
import { Token, TokenType } from "token";
import { prefixParseFn, infixParseFn } from "parser";
import { IntegerLiteral } from "ast/integer-literal";
import { PrefixExpression } from "ast";
import { FunctionLiteral } from "ast/function";

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
    this.registerPrefix(TokenType.FALSE, this.parseBoolean.bind(this));
    this.registerPrefix(TokenType.TRUE, this.parseBoolean.bind(this));
    this.registerPrefix(TokenType.MINUS, this.parsePrefixExpression.bind(this));
    this.registerPrefix(TokenType.LPAREN, this.parseGroupedExpression.bind(this));
    this.registerPrefix(TokenType.FUNCTION, this.parseFunctionLiteral.bind(this));
    this.registerPrefix(TokenType.IF, this.parseIfExpression.bind(this));
    this.registerPrefix(TokenType.STRING, this.parseStringLiteral.bind(this));
    // infix registers
    this.registerInfix(TokenType.PLUS, this.parseInfixExpression.bind(this));
    this.registerInfix(TokenType.LT, this.parseInfixExpression.bind(this));
    this.registerInfix(TokenType.GT, this.parseInfixExpression.bind(this));
    this.registerInfix(TokenType.MINUS, this.parseInfixExpression.bind(this));
    this.registerInfix(TokenType.SLASH, this.parseInfixExpression.bind(this));

    this.registerInfix(TokenType.ASTERISK, this.parseInfixExpression.bind(this));
    this.registerInfix(TokenType.EQ, this.parseInfixExpression.bind(this));
    this.registerInfix(TokenType.NOT_EQ, this.parseInfixExpression.bind(this));
    this.registerInfix(TokenType.LPAREN, this.parseCallExpression.bind(this));
  }

  nextToken() {
    this.currentToken = this.peekToken;
    this.peekToken = this.lexer.nextToken();
  }

  parseProgram(): Program {
    const program = new Program();
    while (this.currentToken.type != TokenType.EOF) {
      const statement = this.parseStatement();
      if (statement != null) {
        program.statements.push(statement);
      }
      this.nextToken();
    }

    return program;
  }
  // ---- parse statement ----
  parseStatement(): Maybe<Statement> {
    switch (this.currentToken.type) {
      case TokenType.LET:
        return this.parseLetStatement();
      case TokenType.RETURN:
        return this.parseReturnStatement();
      default:
        return this.parseExpressionStatement();
    }
  }
  parseLetStatement(): Maybe<LetStatement> {
    const letToken = new LetStatement(this.currentToken);
    if (!this.expectPeek(TokenType.IDENT)) {
      return null;
    }
    const identifier = this.parseIdentifier();
    letToken.name = identifier;
    if (!this.expectPeek(TokenType.ASSIGN)) {
      return null;
    }
    // To-do
    this.nextToken();
    // letToken.value = this.
    letToken.value = this.parseExpression(Precedence.LOWEST);
    if (this.isPeekToken(TokenType.SEMICOLON)) {
      this.nextToken();
    }
    return letToken;
  }
  parseReturnStatement(): Maybe<ReturnStatement> {
    const returnToken = new ReturnStatement(this.currentToken);
    this.nextToken();
    returnToken.returnValue = this.parseExpression(Precedence.LOWEST);
    while (!this.isCurrentToken(TokenType.SEMICOLON)) {
      this.nextToken();
    }
    return returnToken;
  }
  parseExpressionStatement(): Maybe<ExpressionStatement> {
    const expressionStatement = new ExpressionStatement(this.currentToken);
    const expression = this.parseExpression(Precedence.LOWEST);
    if (this.isPeekToken(TokenType.SEMICOLON)) {
      this.nextToken();
    }
    expressionStatement.expression = expression;
    return expressionStatement;
  }
  parseExpression(precedence: Precedence): Maybe<Expression> {
    const prefix = this.prefixParseFns.get(this.currentToken.type);
    if (prefix === undefined) {
      this.noPrefixParseFnError(this.currentToken.type);
      return null;
    }
    let leftExpression = prefix();

    while (!this.isPeekToken(this.currentToken.type) && precedence < this.peekPrecedence()) {
      const infix = this.infixParseFns.get(this.peekToken.type);
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
  parseIdentifier(): Identifier {
    const token = new Identifier(this.currentToken);
    token.value = this.currentToken.literal;
    return token;
  }
  parseIntegerLiteral(): Maybe<IntegerLiteral> {
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
  parsePrefixExpression(): Maybe<PrefixExpression> {
    const expressionStatement = new PrefixExpression(this.currentToken, this.currentToken.literal);
    this.nextToken();
    expressionStatement.right = this.parseExpression(Precedence.PREFIX);
    return expressionStatement;
  }
  parseInfixExpression(left: Expression): Maybe<Expression> {
    const operator = this.currentToken.literal;
    const token = this.currentToken;
    const expression = new InfixExpression(token, operator);
    expression.left = left;
    const precedence = this.currentPrecedence();
    this.nextToken();
    expression.right = this.parseExpression(precedence);
    return expression;
  }
  parseBoolean(): Maybe<BooleanLiteral> {
    return new BooleanLiteral(this.currentToken, this.isCurrentToken(TokenType.TRUE));
  }
  parseGroupedExpression(): Maybe<Expression> {
    this.nextToken();
    const expression = this.parseExpression(Precedence.LOWEST);
    if (!this.expectPeek(TokenType.RPAREN)) {
      return null;
    }
    return expression;
  }
  parseIfExpression(): Maybe<IfExpression> {
    const expression = new IfExpression(this.currentToken);
    if (!this.expectPeek(TokenType.LPAREN)) {
      return null;
    }
    this.nextToken();
    expression.condition = this.parseExpression(Precedence.LOWEST);

    if (!this.expectPeek(TokenType.RPAREN)) {
      return null;
    }

    if (!this.expectPeek(TokenType.LBRACE)) {
      return null;
    }

    expression.consequence = this.parseBlockStatement();
    if (this.isPeekToken(TokenType.ELSE)) {
      this.nextToken();

      if (!this.expectPeek(TokenType.LBRACE)) {
        return null;
      }

      expression.alternative = this.parseBlockStatement();
    }
    return expression;
  }
  parseStringLiteral(): Maybe<StringLiteral> {
    return new StringLiteral(this.currentToken, this.currentToken.literal);
  }
  parseBlockStatement(): Maybe<BlockStatement> {
    const block = new BlockStatement(this.currentToken);

    this.nextToken();

    while (!this.isCurrentToken(TokenType.RBRACE) && !this.isCurrentToken(TokenType.EOF)) {
      const statement = this.parseStatement();
      if (statement !== null) {
        block.statements.push(statement);
      }
      this.nextToken();
    }
    return block;
  }
  parseFunctionLiteral(): Maybe<FunctionLiteral> {
    const functionLiteral = new FunctionLiteral(this.currentToken);
    if (!this.expectPeek(TokenType.LPAREN)) {
      return null;
    }
    functionLiteral.parameters = this.parseFunctionParameters();
    if (!this.expectPeek(TokenType.LBRACE)) {
      return null;
    }
    functionLiteral.body = this.parseBlockStatement();
    return functionLiteral;
  }

  parseFunctionParameters(): Maybe<Identifier[]> {
    const identifiers: Identifier[] = [];

    if (this.isPeekToken(TokenType.RPAREN)) {
      this.nextToken();
      return identifiers;
    }
    this.nextToken();
    const identifier = new Identifier(this.currentToken);
    identifier.value = this.currentToken.literal;
    identifiers.push(identifier);

    while (this.isPeekToken(TokenType.COMMA)) {
      this.nextToken();
      this.nextToken();

      const identifier = new Identifier(this.currentToken);
      identifier.value = this.currentToken.literal;
      identifiers.push(identifier);
    }
    if (!this.expectPeek(TokenType.RPAREN)) {
      return null;
    }
    return identifiers;
  }
  parseCallExpression(fn: Expression): Maybe<CallExpression> {
    const expression = new CallExpression(this.currentToken);
    expression.function = fn;
    expression.arguments = this.parseCallArguments();
    return expression;
  }
  parseCallArguments(): Maybe<Expression[]> {
    const args: Expression[] = [];

    if (this.isPeekToken(TokenType.RPAREN)) {
      this.nextToken();
      return args;
    }

    this.nextToken();
    const arg = this.parseExpression(Precedence.LOWEST);
    if (arg) {
      args.push(arg);
    }
    while (this.isPeekToken(TokenType.COMMA)) {
      this.nextToken();
      this.nextToken();
      const arg = this.parseExpression(Precedence.LOWEST);
      if (arg) {
        args.push(arg);
      }
    }

    if (!this.expectPeek(TokenType.RPAREN)) {
      return null;
    }
    return args;
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
  isError() {
    return this.errorHandler.errors.length > 0;
  }
}

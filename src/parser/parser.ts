import { Program } from "ast/program";
import { VariableStatement } from "ast/variable-statement";
import { Lexer } from "lexer";
import { Token, TokenType } from "token";

export class Parser {
  private lexer: Lexer;
  currentToken: Token;
  peekToken: Token;
  constructor(lexer: Lexer) {
    this.lexer = lexer;
    this.advanceTokens();
    this.advanceTokens();
  }

  private advanceTokens() {
    this.currentToken = this.peekToken;
    this.peekToken = this.lexer.nextToken();
  }

  private parseProgram(): Program {
    let program = new Program();
    this.advanceTokens();

    while (this.currentToken.type !== TokenType.EOF) {
      let statement = null;
      switch (this.currentToken.type) {
        case TokenType.LET:
          statement = this.parseLetStatement();
          break;
        case TokenType.RETURN:
          statement = this.parseReturnStatement();
          break;
        case TokenType.IF:
          statement = this.parseIfStatement();
          break;
      }

      if (statement != null) {
        program.statements.push(statement);
      }
      this.advanceTokens();
    }

    return program;
  }
  //  --------- parseLetStatement ------------ //
  private parseLetStatement(): any {
    this.advanceTokens();
    let identifier = this.parseIdentifier();
    this.advanceTokens();
    if (this.currentToken.type != TokenType.ASSIGN) {
      this.parseError("no equal sign!");
      return null;
    }

    this.advanceTokens();
    let value = this.parseExpression();

    let variableStatement = new VariableStatement(this.currentToken);
    variableStatement.identifier = identifier;
    variableStatement.value = value;
    return variableStatement;
  }
  //  --------- parseIdentifier ------------ //
  private parseIdentifier() {
    let identifier = this.newIdentifierASTNode();
    identifier.token = this.currentToken;
    return identifier;
  }
  //  --------- parseExpression ------------ //
  private parseExpression() {
    if (this.currentToken.type == TokenType.IDENT) {
      if (this.peekToken.type == TokenType.PLUS) {
        return this.parseOperatorExpression();
      }
      if (this.peekToken.type == TokenType.SEMICOLON) {
        return this.parseIntegerLiteral();
      }
    }

    if (this.currentToken.type == TokenType.LPAREN) {
      return this.parseGroupedExpression();
    }
    // [...]
  }
  private parseOperatorExpression() {
    const operatorExpression = this.newOperatorExpression();
    operatorExpression.left = this.parseIntegerLiteral();
    this.advanceTokens();
    operatorExpression.operator = this.currentToken;
    this.advanceTokens();
    operatorExpression.right = this.parseExpression();
    return operatorExpression;
  }
  parseIntegerLiteral() {
    throw new Error("Method not implemented.");
  }

  parseGroupedExpression() {
    throw new Error("Method not implemented.");
  }
  //  --------- parseIfStatement ------------ //
  private parseIfStatement(): any {
    throw new Error("Method not implemented.");
  }
  //  --------- parseReturnStatement ------------ //
  private parseReturnStatement(): any {
    throw new Error("Method not implemented.");
  }

  //  --------- advanceTokens ------------ //

  parseError(arg0: string) {
    throw new Error("Method not implemented.");
  }
  private newIdentifierASTNode() {
    throw new Error("Method not implemented.");
  }
  //  --------- parseIdentifier ------------ //
  private newVariableStatementASTNode() {
    throw new Error("Method not implemented.");
  }
}

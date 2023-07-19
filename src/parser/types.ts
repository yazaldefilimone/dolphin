import { Expression, parseResult } from "ast/base";
export type prefixParseFn = () => parseResult<Expression>;
export type infixParseFn = (expression: Expression) => parseResult<Expression>;

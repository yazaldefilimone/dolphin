import { Expression } from "ast/base";
export type prefixParseFn = () => Expression;
export type infixParseFn = (expression: Expression) => Expression;

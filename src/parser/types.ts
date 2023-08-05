import { Expression } from "ast/base";
import { Maybe } from "utils";

export type prefixParseFn = () => Maybe<Expression>;
export type infixParseFn = (expression: Expression) => Maybe<Expression>;

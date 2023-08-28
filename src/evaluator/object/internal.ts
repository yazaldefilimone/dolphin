import { InternalBoolean, InternalNull } from "evaluator/object";
import { keyboardTable } from "./builtin";

export const internal = {
  TRUE: new InternalBoolean(true),
  FALSE: new InternalBoolean(false),
  NULL: new InternalNull(),
  keyboardTable: keyboardTable,
};

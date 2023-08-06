import { InternalBoolean, InternalNull } from "evaluator/object";

export const internal = {
  TRUE: new InternalBoolean(true),
  FALSE: new InternalBoolean(false),
  NULL: new InternalNull(),
};

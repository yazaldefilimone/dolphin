import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { typeMismatchError, unknownOperatorError, identifierNotFoundError } from "evaluator/errors";
import { InternalBoolean, Integer, InternalNull, EBaseObject } from "evaluator/object";
const default_obj = {
  integer: new Integer(0),
  boolean: new InternalBoolean(false),
  null: new InternalNull(),
};

describe("Evaluator", () => {
  it("evaluator error", () => {
    const tests = [
      {
        input: "5 + true;",
        expected: typeMismatchError(default_obj.integer, default_obj.boolean, "+").inspect(),
      },
      {
        input: "5 + true; 5;",
        expected: typeMismatchError(default_obj.integer, default_obj.boolean, "+").inspect(),
      },
      {
        input: "-true",
        expected: unknownOperatorError(`-${EBaseObject.BOOLEAN}`).inspect(),
      },
      {
        input: "true + false;",
        expected: unknownOperatorError(`${EBaseObject.BOOLEAN} + ${EBaseObject.BOOLEAN}`).inspect(),
      },
      {
        input: "5; true + false; 5",
        expected: unknownOperatorError(`${EBaseObject.BOOLEAN} + ${EBaseObject.BOOLEAN}`).inspect(),
      },
      {
        input: "if (10 > 1) { true + false; }",
        expected: unknownOperatorError(`${EBaseObject.BOOLEAN} + ${EBaseObject.BOOLEAN}`).inspect(),
      },
      {
        input: `if (10 > 1) {
                    if (10 > 1) {
                      return true + false;
                    }
                    return 1;
                  }`,
        expected: unknownOperatorError(`${EBaseObject.BOOLEAN} + ${EBaseObject.BOOLEAN}`).inspect(),
      },
      {
        input: `variable_not_defined`,
        expected: identifierNotFoundError(`variable_not_defined`).inspect(),
      },
    ];

    tests.forEach((tt) => {
      const evaluated = makeSut(tt.input);
      expect(evaluated).not.toBeNull();
      expect(evaluated?.inspect()).toEqual(tt.expected);
    });
  });
});

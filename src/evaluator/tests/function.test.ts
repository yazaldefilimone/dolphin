import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { BaseFunction } from "evaluator/object/function";

describe("Evaluator", () => {
  describe("Function", () => {
    it("should evaluate function", () => {
      const test = `
        fn(x) {
          x + 1;
        };
      `;
      const sut = makeSut(test);
      expect(sut).not.toBeNull();
      expect(sut).toBeInstanceOf(BaseFunction);
      const fn = sut as any;
      expect(fn.parameters).toHaveLength(1);
      expect(fn?.parameters[0].toString()).toBe("x");
      expect(fn.body.statements).toHaveLength(1);
      expect(fn.body.toString()).toBe("{(x + 1)}");
    });
  });
});

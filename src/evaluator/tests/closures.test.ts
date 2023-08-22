import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { BaseFunction } from "evaluator/object/function";

describe("Evaluator", () => {
  describe("Function", () => {
    it("should evaluate closures ", () => {
      const tt = {
        input: `
          let closure = fn(x) {
            fn(y) { x + y };
          };
         let addTwo = closure(2);
          addTwo(3);
        `,
        expected: 5,
      };
      const sut = makeSut(tt.input);
      expect(sut).not.toBeNull();
      expect(sut?.value).toBe(tt.expected);
    });
  });
});

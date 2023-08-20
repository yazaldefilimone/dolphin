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

  it("should evaluate function application", () => {
    const tests = [
      {
        input: `
          let identity = fn(x) { return x; };
          identity(5);
        `,
        expected: 5,
      },
      {
        input: `
          let double = fn(x) { x * 2; };
          double(5);
        `,
        expected: 10,
      },
      {
        input: `
          let add = fn(x, y) { x + y; };
          add(5, 5);
        `,
        expected: 10,
      },
      {
        input: `
          let add = fn(x, y) { x + y; };
          add(5 + 5, add(5, 5));
        `,
        expected: 20,
      },
      {
        input: `
          fn(x) { x; }(5);
        `,
        expected: 5,
      },
    ];
    tests.forEach((tt) => {
      const sut = makeSut(tt.input);
      expect(sut).not.toBeNull();
      expect(sut?.value).toBe(tt.expected);
    });
  });
});

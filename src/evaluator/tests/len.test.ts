import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";

describe("Evaluator", () => {
  describe("len", () => {
    it("should return the length of a string", () => {
      const tests = [
        {
          input: `len("")`,
          expected: 0,
        },
        {
          input: `len("four")`,
          expected: 4,
        },
        {
          input: `len("hello world")`,
          expected: 11,
        },
        {
          input: `len([0,1,2])`,
          expected: 3,
        },
        {
          input: `len(1)`,
          expected: "argument to `len` not supported, got INTEGER",
        },
        {
          input: `len("one", "two")`,
          expected: "wrong number of arguments. got=2, want=1",
        },
      ];

      tests.forEach((tt) => {
        const evaluated = makeSut(tt.input);
        expect(evaluated?.value).toEqual(tt.expected);
      });
    });
  });
});

import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { BaseString } from "evaluator/object";
import { unknownOperatorError } from "evaluator/errors";

const helper = {
  string: (value: string) => new BaseString(value),
};
describe("Evaluator", () => {
  describe("String", () => {
    it("should evaluate string", () => {
      const input = `"Hello World"`;
      const evaluated = makeSut(input);
      expect(evaluated).toBeDefined();
      expect(evaluated).toBeInstanceOf(BaseString);
      expect(evaluated?.inspect()).toMatch("Hello World");
    });
    it("should suporte plus operator in string", () => {
      const tests = [
        {
          input: `"Hello" + " " + "World"`,
          expected: "Hello World",
        },
        {
          input: `"Hello" + " " + "World" + "!"`,
          expected: "Hello World!",
        },
      ];
      tests.forEach((tt) => {
        const evaluated = makeSut(tt.input);
        expect(evaluated).toBeDefined();
        expect(evaluated).toBeInstanceOf(BaseString);
        expect(evaluated?.inspect()).toMatch(tt.expected);
      });
    });
  });
});

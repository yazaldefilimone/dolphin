import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";

describe("Evaluator", () => {
  it("evaluate infix  expression", () => {
    const tests = [
      {
        input: "5 + 5",
        expected: 10,
      },
      {
        input: "5 - 5",
        expected: 0,
      },
      {
        input: "5 * 5",
        expected: 25,
      },
      {
        input: "5 / 5",
        expected: 1,
      },
      {
        input: "5 > 5",
        expected: false,
      },
      {
        input: "5 < 5",
        expected: false,
      },
      {
        input: "5 == 5",
        expected: true,
      },
      {
        input: "5 != 5",
        expected: false,
      },
      {
        input: "true == true",
        expected: true,
      },
    ];
    tests.forEach((test) => {
      console.log(test.input);
      const evaluated = makeSut<number>(test.input);
      expect(evaluated).not.toBeNull();
      expect(evaluated?.value).toEqual(test.expected);
    });
  });
});

import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";

describe("Evaluator", () => {
  it("evaluator let statement", () => {
    const tests = [
      {
        input: `
          let a = 5;
          a;
        `,
        output: 5,
      },
      {
        input: `
          let multi = 5 * 5;
          multi;
        `,
        output: 25,
      },
      {
        input: `
          let a = 5; 
          let b = a;
          b;
        `,
        output: 5,
      },
      {
        input: `
          let a = 5; 
          let b = a;
          let c = a + b + 5;
          c;
        `,
        output: 15,
      },
    ];

    tests.forEach((tt) => {
      const out = makeSut(tt.input);
      expect(out).not.toBeNull();
      expect(out?.inspect()).toEqual(tt.output);
    });
  });
});

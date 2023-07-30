export const concatenationOfString = (initial: string) => {
  let out = initial;
  function plus(string: string[] | string) {
    out = out.concat(...string);
  }
  function get() {
    return out;
  }

  return {
    plus,
    get,
  };
};

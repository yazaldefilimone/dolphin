import { BaseObject, EBaseObject, Integer, InternalError } from "evaluator/object";
type BuiltinFunction = (...args: BaseObject<any>[]) => BaseObject;
export class Builtin implements BaseObject {
  private fn: BuiltinFunction;
  constructor(fn: BuiltinFunction) {
    this.fn = fn;
  }

  type(): EBaseObject {
    return EBaseObject.BUILTIN;
  }

  inspect(): string {
    return "builtin function";
  }

  get value(): BuiltinFunction {
    return this.fn;
  }
}

const len: BuiltinFunction = (...args: BaseObject<string>[]) => {
  if (args.length !== 1) {
    return new InternalError(`wrong number of arguments. got=${args.length}, want=1`);
  }
  const arg = args[0];
  switch (arg.type()) {
    case EBaseObject.STRING: {
      return new Integer(arg?.value.length);
    }
    case EBaseObject.ARRAY: {
      return new Integer(arg?.value.length);
    }
    default: {
      return new InternalError(`argument to \`len\` not supported, got ${arg.type()}`);
    }
  }
};

const keyboardTable = new Map<string, Builtin>();
keyboardTable.set("len", new Builtin(len));
export { keyboardTable };

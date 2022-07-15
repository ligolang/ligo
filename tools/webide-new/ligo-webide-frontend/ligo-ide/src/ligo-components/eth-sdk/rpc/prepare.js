function prepare(parameters, asObject) {
  if (!parameters) {
    return [];
  }
  const { obj } = parameters;
  const keys = Object.keys(obj);
  const values = keys.map(key => {
    const param = obj[key];
    if (param.type === "tuple") {
      return prepare({ obj: param.value }, true);
    }
    if (param.type.endsWith("[]") && !param.value.length) {
      return null;
    }
    if (param.type.startsWith("uint")) {
      let { value } = param;
      if (value === "0") {
        if (key === "gas") {
          value = "500000000";
        } else if (key === "gasPrice") {
          value = "1";
        } else if (key === "blockNumber" || key === "newestBlock") {
          return "latest";
        }
      }
      return `0x${BigInt(value).toString(16)}`;
    }
    return param.value;
  });
  if (asObject) {
    return Object.fromEntries(keys.map((k, i) => [k, values[i]]));
  }
  return values;
}

export default prepare;

type color = 
  { kind: "red" }
| { kind: "white" }
| { kind: "blue" };

type bar = {
  color: color
}

type foo = {
  bar: bar
}

const x = ({bar: {color}}:foo) => {
  let a = 0;
  switch(color.kind) {
    case "red": 
      a = 1;
      break;
    case "white":
      a = 2;
      break;
    case "blue":
      a = 5;
      break
  };
  return a;
}
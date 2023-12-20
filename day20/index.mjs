import fs from "fs";

const file = fs.readFileSync("./input.txt", "utf8");

const lines = file.split("\n").slice(0, -1);

const hdl = {};

for (const line of lines) {
  if (line.startsWith("broadcaster")) {
    const outputs = line.slice(15).split(", ");
    hdl["broadcaster"] = {
      outputs,
      type: "",
      isOn: false,
    };
  } else {
    const type = line[0];

    const [label, remainings] = line
      .slice(1)
      .split("->")
      .map((x) => x.trim());

    const outputs = remainings.split(", ");

    hdl[label] ??= {};
    hdl[label].type = type;
    hdl[label].outputs = outputs;
    hdl[label].isOn = false;

    for (const output of outputs) {
      hdl[output] ??= {};
      hdl[output].inputs ??= {};
      hdl[output].inputs[label] = false;
    }
  }
}

Object.values(hdl).forEach((x) => x.type === "%" && delete x.inputs);

let lowCount = 0;
let highCount = 0;
let rxLowCount = 0;
let pressCount = 0;

const dhHits = {
  tr: [],
  xm: [],
  dr: [],
  nh: [],
};

function press() {
  const queue = [["broadcaster", "low"]];
  lowCount += 1;

  while (queue.length) {
    const [sender, signal] = queue.shift();

    const current = hdl[sender];

    for (const receiver of current.outputs) {
      if (receiver === "dh") {
        Object.entries(hdl["dh"].inputs).forEach(([key, value]) => {
          if (value === "high") {
            dhHits[key].push(pressCount);
          }
          console.log(dhHits, pressCount);
        });
      }

      if (signal === "low") {
        lowCount += 1;
      } else {
        highCount += 1;
      }
      // console.log(`${sender} -${signal}-> ${receiver}`);
      const next = hdl[receiver];

      switch (next.type) {
        case "%":
          if (signal === "high") {
            // it is ignored and nothing happens
          } else {
            if (next.isOn) {
              next.isOn = false;
              queue.push([receiver, "low"]);
            } else {
              next.isOn = true;
              queue.push([receiver, "high"]);
            }
          }
          break;
        case "&":
          next.inputs[sender] = signal;
          const sendLow = Object.values(next.inputs).every((x) => x === "high");
          queue.push([receiver, sendLow ? "low" : "high"]);
          break;
        default:
      }
    }
  }
}

do {
  pressCount++;
  press();
} while (true);

// TODO: rewrite in haskell
// {
//   tr: [ 3739, 3739, 7478, 7478 ],
//   xm: [ 3761, 3761, 7522, 7522 ],
//   dr: [
//     3797, 3797, 3797,
//     3797, 3797, 7594,
//     7594, 7594, 7594,
//     7594
//   ],
//   nh: [
//     3889, 3889, 3889,
//     3889, 3889, 7778,
//     7778, 7778, 7778,
//     7778
//   ]
// }
//
// ghci> foldl1 lcm [(7478 - 3739) ,(7522 - 3761) , (7594 - 3797) , (7778 - 3889)]
// 207652583562007

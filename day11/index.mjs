#!/usr/bin/env node

import fs from "fs";

const data = fs
  .readFileSync("./input.txt", "utf8")
  .split("\n")
  .filter((x) => x)
  .map((x) => x.split(""));

let mx = data.map((x) => x.every((x) => x === "."));
let my = data[0].map((_, i) => data.map((x) => x[i]).every((x) => x === "."));

let galaxies = [];

for (let i = 0; i < data.length; i++) {
  for (let j = 0; j < data[0].length; j++) {
    if (data[i][j] === "#") {
      galaxies.push([i, j]);
    }
  }
}

function solve(multipler) {
  let sum = 0;
  for (let i = 0; i < galaxies.length; i++) {
    for (let j = i + 1; j < galaxies.length; j++) {
      const [x1, y1] = galaxies[i];
      const [x2, y2] = galaxies[j];
      const [xstart, xend] = x1 < x2 ? [x1, x2] : [x2, x1];
      let dx = 0;
      for (let x = xstart + 1; x <= xend; x++) {
        dx += mx[x] ? multipler : 1;
      }

      const [ystart, yend] = y1 < y2 ? [y1, y2] : [y2, y1];
      let dy = 0;
      for (let y = ystart + 1; y <= yend; y++) {
        dy += my[y] ? multipler : 1;
      }
      sum += dx + dy;
    }
  }
  return sum;
}

console.log(solve(2), solve(1000000));

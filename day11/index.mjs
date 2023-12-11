#!/usr/bin/env node

import fs from "fs";

const m = fs
  .readFileSync("input.txt", "utf8")
  .split("\n")
  .slice(0, -1)
  .map((x) => x.split(""));

const scaleX = m.map((x) => x.every((x) => x === "."));
const scaleY = m[0].map((_, i) => m.map((x) => x[i]).every((x) => x === "."));

let galaxies = [];

for (let i = 0; i < m.length; i++) {
  for (let j = 0; j < m[0].length; j++) {
    if (m[i][j] === "#") {
      galaxies.push([i, j]);
    }
  }
}

function solve(scale) {
  let sum = 0;
  for (let i = 0; i < galaxies.length; i++) {
    for (let j = i + 1; j < galaxies.length; j++) {
      const [x1, y1] = galaxies[i];
      const [x2, y2] = galaxies[j];

      const [xstart, xend] = x1 < x2 ? [x1, x2] : [x2, x1];
      let dx = 0;
      for (let x = xstart + 1; x <= xend; x++) {
        dx += scaleX[x] ? scale : 1;
      }

      const [ystart, yend] = y1 < y2 ? [y1, y2] : [y2, y1];
      let dy = 0;
      for (let y = ystart + 1; y <= yend; y++) {
        dy += scaleY[y] ? scale : 1;
      }

      sum += dx + dy;
    }
  }
  return sum;
}

console.log(solve(2), solve(1000000));

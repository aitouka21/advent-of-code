#!/usr/bin/env node

import fs from "fs";

// solve with crossing number algorithm
const m = fs
  .readFileSync("input.txt", "utf-8")
  .split("\n")
  .map((line) => line.split("").map((x) => [x, false]));

let pos = (() => {
  for (let i = 0; i < m.length; i++)
    for (let j = 0; j < m[i].length; j++) if (m[i][j][0] === "S") return [i, j];
})();

let steps = 0;

do {
  const [x, y] = pos;
  m[x][y][1] = true;
  const tile = m[x][y][0];
  if (tile === "S") pos = [x + 1, y];
  if (tile === "|") pos = m[x + 1][y][1] ? [x - 1, y] : [x + 1, y];
  if (tile === "-") pos = m[x][y + 1][1] ? [x, y - 1] : [x, y + 1];
  if (tile === "7") pos = m[x][y - 1][1] ? [x + 1, y] : [x, y - 1];
  if (tile === "L") pos = m[x - 1][y][1] ? [x, y + 1] : [x - 1, y];
  if (tile === "J") pos = m[x - 1][y][1] ? [x, y - 1] : [x - 1, y];
  if (tile === "F") pos = m[x][y + 1][1] ? [x + 1, y] : [x, y + 1];
  steps++;
} while (!m[pos[0]][pos[1]][1]);

let tiles = 0;

// using point in polygon to solve,
// maybe try to solve it using flood fill / Shoelace formula?
for (let i = 0; i < m.length; i++) {
  let crossing = 0;
  for (let j = 0; j < m[i].length; j++) {
    if (m[i][j][1]) {
      if ("|LJS".includes(m[i][j][0])) crossing++;
    } else {
      if (crossing % 2 === 1) tiles++;
    }
  }
}

console.log(steps / 2, tiles);

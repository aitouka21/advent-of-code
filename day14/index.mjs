#!/usr/bin/env node

import fs from "fs";

const arr = fs
  .readFileSync("./input.txt", "utf-8")
  .split("\n")
  .slice(0, -1)
  .map((l) => l.split(""));

function slide(from, direction) {
  const to = Array.from({ length: from.length }).map(() =>
    Array(arr[0].length).fill("."),
  );
  switch (direction) {
    case "N":
      for (let j = 0; j < from[0].length; j++) {
        let top = 0;
        for (let i = 0; i < from.length; i++) {
          if (from[i][j] === "O") to[top++][j] = "O";
          if (from[i][j] === "#") (to[i][j] = "#"), (top = i + 1);
        }
      }
      return to;
    case "W":
      for (let i = 0; i < from.length; i++) {
        let top = 0;
        for (let j = 0; j < from[i].length; j++) {
          if (from[i][j] === "O") to[i][top++] = "O";
          if (from[i][j] === "#") (to[i][j] = "#"), (top = j + 1);
        }
      }
      return to;
    case "S":
      for (let j = 0; j < from[0].length; j++) {
        let top = from.length - 1;
        for (let i = from.length - 1; i >= 0; i--) {
          if (from[i][j] === "O") to[top--][j] = "O";
          if (from[i][j] === "#") (to[i][j] = "#"), (top = i - 1);
        }
      }
      return to;
    case "E":
      for (let i = 0; i < from.length; i++) {
        let top = from[i].length - 1;
        for (let j = from[i].length - 1; j >= 0; j--) {
          if (from[i][j] === "O") to[i][top--] = "O";
          if (from[i][j] === "#") (to[i][j] = "#"), (top = j - 1);
        }
      }
      return to;
  }
}

function totalLoad(arr) {
  let sum = 0;
  for (let i = 0; i < arr.length; i++) {
    for (let j = 0; j < arr[i].length; j++) {
      const current = arr[i][j];
      if (current === "O") {
        sum += arr.length - i;
      }
    }
  }
  return sum;
}

function cycle(arr) {
  return slide(slide(slide(slide(arr, "N"), "W"), "S"), "E");
}
function solveP1(arr) {
  return totalLoad(slide(arr, "N"));
}
function solveP2(arr) {
  const m = new Map();
  let next = arr;
  for (let i = 0; i < 1000000000; i++) {
    next = cycle(next);
    const key = next.map((l) => l.join("")).join("\n");
    if (m.has(key)) {
      const period = i - m.get(key);
      const remaining = 1000000000 - i;
      const final = remaining % period;
      for (let [k, v] of m.entries())
        if (v === final + m.get(key) - 1)
          return totalLoad(k.split("\n").map((l) => l.split("")));
    } else {
      m.set(key, i);
    }
  }
}

const p1 = solveP1(arr);
const p2 = solveP2(arr);
console.log(p1, p2);

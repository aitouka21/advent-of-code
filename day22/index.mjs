import fs from "fs";

const bricks = fs
  .readFileSync("input.txt", "utf-8")
  .split("\n")
  .slice(0, -1)
  .map((line) => line.split("~").map((x) => x.split(",").map((x) => +x)))
  .map(([from, to]) => ({ from, to }))
  .sort((a, b) => a.from[2] - b.from[2]);

const check = (a, b) =>
  a.from[0] <= b.to[0] &&
  a.to[0] >= b.from[0] &&
  a.from[1] <= b.to[1] &&
  a.to[1] >= b.from[1];

const floors = [];

for (const brick of bricks) {
  let [zIndex, zLength] = [brick.from[2], brick.to[2] - brick.from[2] + 1];

  while (--zIndex > 0) {
    const floor = floors[zIndex];
    if (floor) {
      const supports = floor.filter((x) => check(x, brick));
      if (supports.length) {
        brick.supports = supports;
        (floors[zIndex + zLength] ??= []).push(brick);
        supports.forEach((x) => (x.supporting ??= []).push(brick));
        break;
      }
    }
  }
  if (zIndex === 0) (brick.supports = []), (floors[zLength] ??= []).push(brick);
}

const count1 = bricks.reduce((acc, x) => acc + ((x.supporting ??= []).every(x => x.supports.length > 1)), 0);

let count2 = 0;

for (const brick of bricks) {
  bricks.forEach(b => delete b.disintegrated);
  const queue = [brick];
  do {
    const brick = queue.shift();
    brick.disintegrated = true;
    const chained = brick.supporting.filter(x => x.supports.every(x => x.disintegrated));
    count2 += chained.length, queue.push(...chained);
  } while (queue.length);
}

console.log(count1, count2);

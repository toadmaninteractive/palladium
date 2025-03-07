export function getRandomInt(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

export function getRandomArbitraryFloat(min, max) {
    return Math.random() * (max - min) + min;
  }
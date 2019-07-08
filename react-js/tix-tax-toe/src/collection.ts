export const findFirst = <A>(arr: Array<A>, pred: (_: A) => Boolean): A | null => {
  for (let i = 0; i < arr.length; i++) {
    if (pred(arr[i])) {
      return arr[i];
    }
  }
  return null;
};

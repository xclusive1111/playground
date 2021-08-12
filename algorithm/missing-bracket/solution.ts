const solution = (angle: string): string => {
    if (!angle) return '';

    const arr = angle.split('');
    let close = 0;
    let open = 0;

    const findOpen = (i: number): number => {
        const idx = i;
        for (; i >= 0; i--) {
            if (arr[i] === '<') {
                arr[idx] = arr[i] = '-';
                return 0;
            }
        }

        return 1;
    }

    const findClose = (i: number): number => {
        const idx = i;
        for (; i < arr.length; i++) {
            if (arr[i] === '>') {
                arr[idx] = arr[i] = '-';
                return 0;
            }
        }

        return 1;
    }

    for (var i = 0; i < arr.length; i++) {
        const c = arr[i];
        if (c === undefined) continue;
        if (c === '>') open += findOpen(i);
        if (c === '<') close += findClose(i);
    }

    const _close = new Array(close).fill('>').join('');
    const _open = new Array(open).fill('<').join('');

    return _open + angle + _close;
}

console.log(solution('>><<'));

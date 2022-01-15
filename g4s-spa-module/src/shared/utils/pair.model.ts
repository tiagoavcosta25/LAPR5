export class Pair {
    key: string;
    value: number;

    constructor(key: string, value: number) {
        this.key = key;
        this.value = value;
    }

    increment() {
        this.value = this.value + 1;
    }
}

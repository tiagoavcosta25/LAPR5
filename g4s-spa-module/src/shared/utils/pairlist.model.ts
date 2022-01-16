import { Pair } from "./pair.model";

export class Pairlist {
    pairlist: Pair[];

    constructor() {
        this.pairlist = [];
    }

    addOrChange(key: string): void {
        let index = this.checkIfInList(key);
        if(index == -1) {
            this.pairlist.push(new Pair(key, 1)); 
        } else {
            this.pairlist[index].increment();
        } 
    }

    checkIfInList(key: string): number {
        for(let position = 0; position < this.pairlist.length; position++) {
            if(this.pairlist[position].key == key) {
                return position;
            }
        }
        return -1;
    }

    length(): number {
        return this.pairlist.length;
    }
}

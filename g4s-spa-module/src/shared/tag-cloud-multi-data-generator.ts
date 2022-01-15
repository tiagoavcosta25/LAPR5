import { CloudData } from 'angular-tag-cloud-module';
import { Pairlist } from './utils/pairlist.model';

export function prepareDataForTagCloudMulti(tags: Pairlist): CloudData[] {
    const cd: CloudData[] = [];

    let biggest = 0;

    for(let t of tags.pairlist) {
      if(t.value > biggest) {
        biggest = t.value;
      }
    }

    for (let i = 0; i < tags.length(); i++) {
      let color: string = "";

      let weight = Math.ceil(tags.pairlist[i].value / biggest * 10);

      let text = tags.pairlist[i].key;
      let rotate = 0;

      // randomly rotate some elements (less probability)
      if (Math.random() >= 0.8) {
        const plusMinus = Math.random() >= 0.5 ? '' : '-';
        rotate = Math.floor(Math.random() * Number(`${plusMinus}20`) + 1);
      }

      // randomly set color attribute
      color = '#' + Math.floor(Math.random() * 16777215).toString(16);

      // set random weight

      const el: CloudData = {
        text: text + " - " + tags.pairlist[i].value,
        weight: weight,
        color: color,
        rotate: rotate
      };

      cd.push(el);
    }

    return cd;
  }
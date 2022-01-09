import { CloudData } from 'angular-tag-cloud-module';

export function prepareDataForTagCloud(tags: string[]): CloudData[] {
    const cd: CloudData[] = [];

    for (let i = 0; i < tags.length; i++) {
      let color: string = "";
      let weight = 5;
      let text = tags[i];
      let rotate = 0;

      // randomly rotate some elements (less probability)
      if (Math.random() >= 0.8) {
        const plusMinus = Math.random() >= 0.5 ? '' : '-';
        rotate = Math.floor(Math.random() * Number(`${plusMinus}20`) + 1);
      }

      // randomly set color attribute
      color = '#' + Math.floor(Math.random() * 16777215).toString(16);

      // set random weight
      weight = Math.floor(Math.random() * (15 - 5 + 1) + 5);

      const el: CloudData = {
        text: text,
        weight: weight,
        color: color,
        rotate: rotate
      };

      cd.push(el);
    }

    return cd;
  }
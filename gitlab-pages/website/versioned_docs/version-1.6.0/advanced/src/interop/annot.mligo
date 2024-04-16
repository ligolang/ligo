type animal =
| [@annot memory] Elephant
| [@annot face] Dog
| [@annot fish] Cat
type artist = {
  [@annot style] genre: string;
  [@annot from] since: timestamp;
  [@annot performer] name: string;
}
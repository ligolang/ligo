import chalk from "chalk";
import { getColor } from "~/base-components/ui-components";

chalk.enabled = true;
chalk.level = 2;

const hex = (value) => {
  const hexValue = Number(value || 0).toString(16);
  return hexValue.length === 1 ? `0${hexValue}` : hexValue;
};

export default function colorCommand(cmdString, colorStr = "#000") {
  const primaryColorRgb = getColor(colorStr);
  const rgb = primaryColorRgb.match(/\((\d+),\s*(\d+),\s*(\d+)\)/);
  const primaryColor = `#${hex(rgb[1])}${hex(rgb[2])}${hex(rgb[3])}`;

  return chalk.hex(primaryColor).bold(cmdString);
}

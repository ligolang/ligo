type color = Blue | Green

type preferences = {
  color : color;
  other : int
}

type account = {
  id          : int;
  preferences : preferences
}
let change_color_preference (account : account) (color : color) : account =
  { account with preferences.color = color }
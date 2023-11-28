import { useColorMode } from "@docusaurus/theme-common";
import React from "react";
const ThemeReset = () => {
  const { setColorMode } = useColorMode();

  React.useEffect(() => {
    if (localStorage.getItem("theme-reset") !== "true") {
      localStorage.removeItem("theme");
      localStorage.setItem("theme-reset", "true");
      setColorMode("dark");
    }
  }, []);
  return null;
};

export default ThemeReset;

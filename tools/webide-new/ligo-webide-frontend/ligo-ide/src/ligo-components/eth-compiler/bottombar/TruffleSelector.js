import React from "react";

import { DockerImageSelector } from "~/base-components/docker";
import { LocalProjectManager } from "~/base-components/workspace";
import compilerManager from "../compilerManager";

export default function () {
  const [framework, setFramework] = React.useState("");
  const [selected, onSelected] = React.useState("");

  React.useEffect(LocalProjectManager.effect("settings:framework", setFramework), []);
  React.useEffect(
    LocalProjectManager.effect(
      `settings:compilers.${process.env.COMPILER_VERSION_KEY}`,
      onSelected
    ),
    []
  );

  if (!framework.endsWith("-docker")) {
    return null;
  }

  return (
    <DockerImageSelector
      channel={compilerManager.truffle}
      disableAutoSelection
      size="sm"
      icon="fas fa-cookie"
      title={`${process.env.COMPILER_NAME}`}
      noneName={`${process.env.COMPILER_NAME}`}
      modalTitle={`${process.env.COMPILER_NAME} Manager`}
      downloadingTitle={`Downloading ${process.env.COMPILER_NAME}`}
      selected={selected}
      onSelected={(v) =>
        LocalProjectManager.instance.projectSettings?.set(
          `compilers.${process.env.COMPILER_VERSION_KEY}`,
          v
        )
      }
    />
  );
}

import React from "react";

import { DropdownToolbarButton } from "~/base-components/ui-components";

import { BaseProjectManager } from "~/base-components/workspace";

export default function ScriptsButton({ projectManager }) {
  const [isNodeProject, setNodeProject] = React.useState(false);
  const [options, setOptions] = React.useState([]);

  React.useEffect(
    BaseProjectManager.effect("settings:framework", framework => {
      const isNodeProject = !framework.endsWith("-docker");
      setNodeProject(isNodeProject);
    }),
    []
  );

  React.useEffect(() => {
    if (!isNodeProject) {
      return;
    }
    projectManager
      .readPackageJson()
      .then(packageJson => {
        const { scripts } = packageJson;
        if (scripts) {
          return Object.keys(scripts).map(key => ({
            key,
            onClick: () => {
              const npmClient = projectManager.projectSettings.get("npmClient");
              const npmRun = npmClient === "yarn" ? npmClient : `${npmClient} run`;
              projectManager.executeInTerminal(`${npmRun} ${key}`);
            },
          }));
        }
        return [];
      })
      .then(setOptions)
      .catch();
  }, [isNodeProject]);

  if (!isNodeProject) {
    return null;
  }

  return <DropdownToolbarButton id="scripts" icon="fas fa-code" options={options} />;
}

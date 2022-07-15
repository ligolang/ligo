import React from "react";
import { Screen } from "~/base-components/ui-components";

export default function ProjectInvalid({ projectRoot, children }) {
  return (
    <Screen>
      <h4 className="display-4">Project Not Found</h4>
      <p className="lead">
        Cannot find the project <kbd>{projectRoot}</kbd>
      </p>
      {children && <hr />}
      <span>{children}</span>
    </Screen>
  );
}

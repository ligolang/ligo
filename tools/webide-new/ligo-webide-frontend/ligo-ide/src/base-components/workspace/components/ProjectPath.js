import React from "react";

export default function ProjectPath({ projectRoot, remote }) {
  return (
    <kbd key="project-path-remote">
      <span className="d-inline-block mr-1">
        <i className="fas fa-cloud" />
      </span>
      {projectRoot}
    </kbd>
  );
}

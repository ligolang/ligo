import { useEffect, useState } from "react";
import { Redirect } from "react-router-dom";
import fileOps from "~/base-components/file-ops";
import { ProjectManager } from "~/base-components/workspace/ProjectManager";
import redux from "~/base-components/redux";
import { LoadingScreen } from "~/base-components/ui-components";

export interface OpenProjectProps {
  projectLink: string;
}

function OpenProject({ projectLink }: OpenProjectProps) {
  const [loading, setLoading] = useState(true);
  const [pm, setPm] = useState("");

  useEffect(() => {
    async function loadProject() {
      const projectData = await fileOps.loadGistProject(projectLink);

      // const projectNames = await fileOps.getProjectNames();
      // /* eslint-disable */
      // const config = JSON.parse(projectData["/config.json"].content || "{}");
      // config.gistId = projectLink;
      // const configProjectName = config.projectName ? config.projectName : projectLink;
      // const exitstingNames = projectNames.filter((pn) => pn.includes(configProjectName));
      // const projectName: string =
      //   exitstingNames.length === 0
      //     ? configProjectName
      //     : `${configProjectName}(${exitstingNames.length})`;
      // config.projectName = projectName;
      // projectData["/config.json"].content = JSON.stringify(config);
      // /* eslint-enable */

      const Manager = ProjectManager;
      const created = await Manager.openProject(projectData, projectLink, undefined);

      redux.dispatch("ADD_PROJECT", {
        type: "local",
        project: created,
      });

      setPm(created.id);
      setLoading(false);
    }
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    loadProject();
  }, [projectLink]);

  return loading ? <LoadingScreen /> : <Redirect to={`/local/${pm}`} />;
}

export default OpenProject;

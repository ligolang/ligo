import { useEffect, useState } from "react";
import { Redirect } from "react-router-dom";
import { useSelector } from "react-redux";
import fileOps from "~/base-components/file-ops";
import { ProjectManager } from "~/base-components/workspace/ProjectManager";
import redux from "~/base-components/redux";
import { LoadingScreen } from "~/base-components/ui-components";
import OpenExistingProjectModal from "./OpenExistingProjectModal";

export interface OpenProjectProps {
  gistId: string;
}

const OpenProject = ({ gistId }: OpenProjectProps) => {
  const [loading, setLoading] = useState(true);
  const [pm, setPm] = useState("");
  const [existingProject, setExistingProject] = useState<
    { path: string; gistId: string } | undefined
  >(undefined);
  const [action, setAction] = useState<"open" | "update" | "copy" | undefined>(undefined);
  /* eslint-disable */
  // @ts-ignore
  const projects = useSelector((state) => state.projects);
  /* eslint-enable */

  const copy = async (gId: string) => {
    const projectData = await fileOps.loadGistProject(gId);

    const Manager = ProjectManager;
    const created = await Manager.openProject({
      type: "rawgist",
      obj: projectData,
      gistId: gId,
      name: undefined,
    });

    redux.dispatch("ADD_PROJECT", {
      type: "local",
      project: created,
    });

    setPm(created.id);
    setLoading(false);
  };

  const update = async (projectContext: { path: string; gistId: string }) => {
    await fileOps.deleteDirectory(projectContext.path);
    const projectData = await fileOps.loadGistProject(projectContext.gistId);

    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const config = JSON.parse(projectData["/config.json"].content || "{}");
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    config.gistId = projectContext.gistId;
    projectData["/config.json"].content = JSON.stringify(config);

    const Manager = ProjectManager;
    const created = await Manager.processProject({
      type: "gist",
      name: projectContext.path.replace(".workspaces/", ""),
      obj: projectData,
    });

    setPm(created.id);
    setLoading(false);
  };

  useEffect(() => {
    async function loadProject() {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
      const paths: string[] = projects
        .get("local")
        .toJS()
        // eslint-disable-next-line @typescript-eslint/no-unsafe-return
        .map((p: { path: any }) => p.path);

      const gistIds = await Promise.all(
        paths.map((p) =>
          fileOps
            .readFile(`${p}/config.json`)
            .then((content) => {
              // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access
              const gId: string = JSON.parse(content).gistId;
              return { path: p, gistId: gId };
            })
            .catch(() => undefined)
        )
      );

      const alreadyExistedProject = gistIds.find((gi) => !!gi && gi.gistId === gistId);

      if (alreadyExistedProject) {
        setExistingProject(alreadyExistedProject);
        return;
      }

      await copy(gistId);
    }
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    loadProject();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [gistId]);

  useEffect(() => {
    if (action === "open" && existingProject) {
      setPm(existingProject.path.replace(".workspaces/", ""));
      setLoading(false);
      return;
    }
    if (action === "copy" && existingProject) {
      // eslint-disable-next-line @typescript-eslint/no-floating-promises
      copy(existingProject.gistId);
      return;
    }
    if (action === "update" && existingProject) {
      // eslint-disable-next-line @typescript-eslint/no-floating-promises
      update(existingProject);
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [action]);

  if (existingProject && !action) {
    return (
      <OpenExistingProjectModal
        isOpen
        projectInfo={existingProject}
        callback={(a) => setAction(a)}
      />
    );
  }

  if (loading) {
    return <LoadingScreen />;
  }

  return <Redirect to={`/local/${pm}`} />;
};

export default OpenProject;

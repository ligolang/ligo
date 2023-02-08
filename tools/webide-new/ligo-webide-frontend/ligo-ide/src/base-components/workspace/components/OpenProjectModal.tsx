import { forwardRef, useImperativeHandle, useRef, useState } from "react";
import fileOps from "~/base-components/file-ops";

import {
  Modal,
  DebouncedFormGroup,
  ButtonOptions,
  DropdownInput,
  Button,
} from "~/base-components/ui-components";

import notification from "~/base-components/notification";

import { ProjectManager } from "../ProjectManager";
import actions from "../actions";
import { RawGistProjectType, ProcessGitProject } from "../ProjectManager/ProjectManager";
import { validGistId, validGit, validName } from "~/components/validators";

const OpenProjectModal = forwardRef((_, ref) => {
  actions.openProjectModal = ref;
  const modalRef = useRef<Modal>(null);
  const [name, setName] = useState("");
  const [gistId, setGistId] = useState("");
  const [gitUrl, setGitUrl] = useState("");
  const [loading, setLoading] = useState(false);
  const [loadingBranches, setLoadingBranches] = useState(false);
  const [projectType, setProjectType] = useState("gist");
  const [branches, setBranches] = useState<string[]>([]);
  const [selectedBranch, setSelectedBranch] = useState<string | undefined>(undefined);
  const [token, setToken] = useState<string | undefined>(undefined);
  const resolverRef = useRef<
    ((_: { id: string; author: string; path: string; name: string }) => void) | undefined
  >(undefined);

  const loadBranches = async () => {
    if (gitUrl) {
      setLoadingBranches(true);
      const { main, refs } = await fileOps.loadGitBranches(gitUrl, token);
      setSelectedBranch(main);
      setBranches(refs);
      setLoadingBranches(false);
    }
  };

  useImperativeHandle(ref, () => ({
    async openModal() {
      setName("");
      setGistId("");
      setLoading(false);
      await modalRef.current?.openModal();
      return new Promise((resolve) => {
        resolverRef.current = resolve;
      });
    },
  }));

  const onOpenProject = async () => {
    setLoading(true);

    if ((projectType === "gist" && gistId === "") || (projectType === "git" && gitUrl === "")) {
      return;
    }

    let created:
      | {
          id: string;
          author: string;
          path: string;
          name: string;
        }
      | false = false;

    if (projectType === "gist") {
      const obj = await fileOps.loadGistProject(gistId).catch((e: Error) => {
        notification.error("Gist load error", e.message);
      });

      if (!obj) {
        setLoading(false);
        return;
      }

      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
      const config = JSON.parse(obj["/config.json"].content || "{}");
      // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
      config.gistId = gistId;
      obj["/config.json"].content = JSON.stringify(config);

      created = await openProject({
        type: "rawgist",
        obj,
        gistId,
        name: name === "" ? undefined : name,
      });
    }

    if (projectType === "git" && name !== "") {
      created = await openProject({
        type: "git",
        name,
        gitLink: gitUrl,
        branch: selectedBranch,
        token,
      });
    }

    if (created && resolverRef.current) {
      modalRef.current?.closeModal().catch((me: Error) => {
        console.error(me);
      });
      resolverRef.current(created);
      setName("");
      setGistId("");
      setGitUrl("");
      setLoading(false);
      setSelectedBranch(undefined);
      setToken(undefined);
      setBranches([]);
    } else {
      setLoading(false);
    }
  };

  const openProject = async (projectData: RawGistProjectType | ProcessGitProject) => {
    try {
      const Manager = ProjectManager;
      const created = await Manager.openProject(projectData);
      notification.success("Successful", `New project <b>${created.id}</b> is loaded.`);
      return created;
    } catch (e: any) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
      notification.error("Cannot Create the Project", e.message);
      return false;
    }
  };

  const mapKeyToOption = (key: string) => {
    return {
      id: key,
      display: (
        <div className="w-100 d-flex align-items-center justify-content-between">
          <code className="text-overflow-dots mr-1">{key}</code>
        </div>
      ),
      onClick: () => setSelectedBranch(key),
    };
  };

  return (
    <Modal
      ref={modalRef}
      title="Load project"
      textConfirm="Load"
      onConfirm={onOpenProject}
      pending={loading && "Loading..."}
      confirmDisabled={
        (projectType === "gist" && (gistId === "" || !!validGistId(gistId))) ||
        (projectType === "git" && (name === "" || gitUrl === "" || !!validGit(gitUrl))) ||
        !!(name && validName(name))
      }
    >
      <DebouncedFormGroup
        label={`Project name${projectType === "gist" ? " (optional)" : ""}`}
        placeholder="Name"
        value={name}
        onChange={(n: string) => setName(n)}
        validator={validName}
      />
      <br />
      <ButtonOptions
        size=""
        className="mb-0"
        options={[
          { key: "gist", text: "Gist Projects" },
          { key: "git", text: "Git Projects" },
        ]}
        selected={projectType}
        onSelect={(t: string) => setProjectType(t)}
      />
      <br />
      {projectType === "gist" && (
        <DebouncedFormGroup
          label={
            'Gist id (use gist id from your gist link or simply open it addind "/share/{gist_id}" to the host name'
          }
          placeholder="Id"
          value={gistId}
          onChange={(l: string) => setGistId(l)}
          validator={validGistId}
        />
      )}
      {projectType === "git" && (
        <DebouncedFormGroup
          label="Url of git repository"
          placeholder="Url"
          value={gitUrl}
          onChange={(l: string) => setGitUrl(l)}
          validator={validGit}
        />
      )}
      {projectType === "git" && gitUrl && !validGit(gitUrl) && (
        <>
          <DebouncedFormGroup
            label={
              <>
                <p>Personal Access Token (for private repos){"\n"}</p>
                <p>
                  You can find instrunctions on how to create it here:{" "}
                  <a
                    // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                    href="////docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token"
                    target="_blank"
                    rel="noreferrer"
                  >
                    GitHub
                  </a>
                  {", "}
                  <a
                    // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                    href="////docs.gitlab.com/ee/user/profile/personal_access_tokens.html"
                    target="_blank"
                    rel="noreferrer"
                  >
                    GitLab
                  </a>
                  {", "}
                  <a
                    // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                    href="////support.atlassian.com/bitbucket-cloud/docs/app-passwords/"
                    target="_blank"
                    rel="noreferrer"
                  >
                    Bitbucket
                  </a>
                </p>
              </>
            }
            placeholder="Token"
            value={token}
            onChange={(t: string) => setToken(t)}
          />
          {/* eslint-disable-next-line @typescript-eslint/ban-ts-comment */}
          {/* @ts-ignore */}
          <div style={{ display: "flex", "align-items": "flex-end" }}>
            <DropdownInput
              label="Branch (load branches, or default will be chosen)"
              placeholder="Default"
              inputClassName="code"
              addon={
                <span
                  key={`key-icon-${`fas ${
                    loadingBranches ? "fa-spinner fa-pulse" : "fa-code-branch"
                  }`.replace(/\s/g, "-")}`}
                >
                  <i
                    className={`fas ${loadingBranches ? "fa-spinner fa-pulse" : "fa-code-branch"}`}
                  />
                </span>
              }
              options={branches.map(mapKeyToOption)}
              renderText={(option: { id: string }) => (option ? <code>{option.id}</code> : null)}
              value={selectedBranch}
              onChange={(o: { id: string }) => setSelectedBranch(o.id)}
              style={{ flex: "auto" }}
            />
            <Button
              color="warning"
              className="ml-2 mb-3"
              style={{ height: "fit-content" }}
              onClick={() => {
                loadBranches().catch((e: Error) => {
                  notification.error("Branches loading error", e.message);
                  setLoadingBranches(false);
                });
              }}
            >
              Load Branches
            </Button>
          </div>
        </>
      )}
    </Modal>
  );
});

export default OpenProjectModal;

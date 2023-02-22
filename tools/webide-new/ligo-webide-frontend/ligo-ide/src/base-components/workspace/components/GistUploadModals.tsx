import React, { forwardRef, useState, useRef, useImperativeHandle } from "react";
import { useDispatch, useSelector } from "react-redux";
import fileOps from "~/base-components/file-ops";

import {
  Modal,
  DebouncedFormGroup,
  Button,
  UncontrolledTooltip,
} from "~/base-components/ui-components";

import notification from "~/base-components/notification";
import FileTree from "~/base-components/filetree";
import type ProjectManager from "../ProjectManager/ProjectManager";

interface GistUploadModalsProps {
  fileTreeRef: React.RefObject<typeof FileTree>;
  projectManager: ProjectManager;
}

const GistUploadModals = forwardRef(
  ({ fileTreeRef, projectManager }: GistUploadModalsProps, ref) => {
    const modalRef = useRef<Modal>(null);
    const [loading, setLoading] = useState(false);
    const [gistLink, setGistLink] = useState("");
    const [newToken, setNewToken] = useState("");
    const [gistId, setGistId] = useState("");
    const [isUpdatable, setIsUpdatable] = useState(false);

    useImperativeHandle(ref, () => ({
      async openModal(gId?: string) {
        setNewToken("");
        setGistLink("");
        setLoading(false);
        setGistId(gId || "");
        setIsUpdatable(!!gId);
        await modalRef.current?.openModal();
      },
    }));

    /* eslint-disable */
    // @ts-ignore
    const token: string = useSelector((state) => state.gistToken);
    /* eslint-enable */

    const dispatch = useDispatch();

    const gistIdFromLink = (str: string) => {
      const idr = /[0-9A-Fa-f]{8,}/;
      const match = idr.exec(str);
      return match ? match[0] : null;
    };

    const onCreate = async (action: "update" | "create") => {
      if (gistLink !== "") {
        setGistLink("");
        modalRef.current?.closeModal().catch((me: Error) => {
          console.error(me);
        });
        return;
      }

      const fileTree = fileTreeRef.current;

      if (fileTree === null) {
        modalRef.current?.closeModal().catch((me: Error) => {
          console.error(me);
        });
        notification.error("Gist load error", "File tree is not available");
        return;
      }

      /* eslint-disable */
      // @ts-ignore
      const root: string = fileTree.rootNode[0].name;
      /* eslint-enable */

      setLoading(true);

      const link = await fileOps
        .uploadGistProject(token, root, action === "update" && gistId !== "" ? gistId : undefined)
        .catch((e: Error) => {
          notification.error("Gist load error", e.message);
        });

      if (!link) {
        setLoading(false);
        return;
      }

      if (action === "update") {
        setLoading(false);
        modalRef.current?.closeModal().catch((me: Error) => {
          console.error(me);
        });
        return;
      }

      const gId = gistIdFromLink(link);

      if (projectManager.projectSettings) {
        await projectManager.projectSettings.set("gistId", gId).catch((e: Error) => {
          notification.error("GistId saving error", e.message);
        });
      }

      setLoading(false);
      setGistLink(link);
    };

    const getGistId = (gId: string, gLink: string) => {
      if (gLink !== "") {
        return gistIdFromLink(gLink);
      }
      if (gId !== "") {
        return gId;
      }
      throw new Error("No gist id or gist link");
    };

    return (
      <Modal
        ref={modalRef}
        title="Upload Workspace to Gist"
        textConfirm={newToken !== "" ? "Save" : gistLink !== "" ? "Ok" : "Upload"}
        pending={loading && "Uploading..."}
        confirmDisabled={!token}
        onConfirm={
          newToken === ""
            ? () => onCreate("create")
            : () => {
                dispatch({ type: "SET_GIST_TOKEN", payload: newToken });
                setNewToken("");
              }
        }
        onCancel={() => {
          setLoading(false);
          setNewToken("");
          setGistLink("");
          setGistId("");
          return true;
        }}
        textActions={isUpdatable && gistLink === "" ? ["Update"] : ""}
        onActions={isUpdatable && gistLink === "" ? [() => onCreate("update")] : []}
      >
        {(gistLink !== "" || gistId !== "") && (
          <>
            {getGistId(gistId, gistLink) !== null && (
              <div className="d-flex">
                <Button
                  size="sm"
                  color="default"
                  id="btn-gistId"
                  key="btn-gistId"
                  className="flex-none w-5 flex-column align-items-center mr-1"
                  /* eslint-disable */
                  // @ts-ignore
                  onClick={() => navigator.clipboard.writeText(getGistId(gistId, gistLink))}
                  /* eslint-enable */
                  disabled={false}
                >
                  <span key="clipboard">
                    <i className="fas fa-clipboard" />
                  </span>
                </Button>
                <UncontrolledTooltip
                  trigger="hover"
                  delay={0}
                  placement="bottom"
                  target="btn-gistId"
                >
                  Copy to clipboard
                </UncontrolledTooltip>
                <p className="mt-1">
                  Your Gist id <kbd>{getGistId(gistId, gistLink)}</kbd>
                </p>
              </div>
            )}

            <div className="d-flex">
              <Button
                size="sm"
                color="default"
                id="btn-gistLink"
                key="btn-gistLink"
                className="flex-none w-5 flex-column align-items-center mr-1"
                // eslint-disable-next-line @typescript-eslint/no-misused-promises
                onClick={() =>
                  navigator.clipboard.writeText(
                    `https://gist.github.com/${getGistId(gistId, gistLink) || ""}`
                  )
                }
                disabled={false}
              >
                <span key="clipboard">
                  <i className="fas fa-clipboard" />
                </span>
              </Button>
              <UncontrolledTooltip
                trigger="hover"
                delay={0}
                placement="bottom"
                target="btn-gistLink"
              >
                Copy to clipboard
              </UncontrolledTooltip>
              <p>
                Gist link{" "}
                <a
                  href={`//gist.github.com/${getGistId(gistId, gistLink) || ""}`}
                  target="_blank"
                  rel="noreferrer"
                >
                  {`https://gist.github.com/${getGistId(gistId, gistLink) || ""}`}
                </a>
              </p>
            </div>

            {getGistId(gistId, gistLink) !== null && (
              <div className="d-flex">
                <Button
                  size="sm"
                  color="default"
                  id="btn-gistIdeLink"
                  key="btn-gistIdeLink"
                  className="flex-none w-5 flex-column align-items-center mr-1"
                  // eslint-disable-next-line @typescript-eslint/no-misused-promises
                  onClick={() =>
                    navigator.clipboard.writeText(
                      // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                      `https://${window.location.host}/share/${getGistId(gistId, gistLink)}`
                    )
                  }
                  disabled={false}
                >
                  <span key="clipboard">
                    <i className="fas fa-clipboard" />
                  </span>
                </Button>
                <UncontrolledTooltip
                  trigger="hover"
                  delay={0}
                  placement="bottom"
                  target="btn-gistIdeLink"
                >
                  Copy to clipboard
                </UncontrolledTooltip>
                <p>
                  Web IDE link{" "}
                  <a
                    // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                    href={`//${window.location.host}/share/${getGistId(gistId, gistLink)}`}
                    target="_blank"
                    rel="noreferrer"
                  >
                    {/* eslint-disable-next-line @typescript-eslint/restrict-template-expressions */}
                    {`https://${window.location.host}/share/${getGistId(gistId, gistLink)}`}
                  </a>
                </p>
              </div>
            )}
            {gistLink !== "" && (
              <p>
                Its id will be stored in config. You will be able to use it to update files in gist.
              </p>
            )}
            <br />
          </>
        )}
        {gistLink === "" && (
          <>
            <DebouncedFormGroup
              label={
                <p>
                  To upload your project you need to add{" "}
                  <a
                    // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                    href="////docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token"
                    target="_blank"
                    rel="noreferrer"
                  >
                    github token
                  </a>
                  , or leave the default one to create a gist without a Github account. Currenty you
                  are going to use {token === "default" ? "" : <kbd>{token}</kbd>}{" "}
                  {token === "default" ? <b>default</b> : <b>custom</b>} token. You can change saved
                  token below or reset to default.
                </p>
              }
              maxLength="100"
              value={newToken}
              placeholder="Token"
              onChange={(t: string) => setNewToken(t)}
            >
              <Button
                color="warning"
                className="btn-left-round-zero"
                onClick={() => {
                  dispatch({ type: "SET_GIST_TOKEN", payload: "default" });
                }}
              >
                Reset
              </Button>
            </DebouncedFormGroup>
            {gistId !== "" && (
              <div>
                You can simply update project in gist using <b>Update</b> button.
              </div>
            )}
          </>
        )}
      </Modal>
    );
  }
);

export default GistUploadModals;

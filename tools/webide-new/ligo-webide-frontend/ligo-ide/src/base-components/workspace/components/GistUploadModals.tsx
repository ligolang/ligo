import React, { useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import fileOps from "~/base-components/file-ops";

import { Modal, DebouncedFormGroup } from "~/base-components/ui-components";

import notification from "~/base-components/notification";
import FileTree from "~base-components/filetree";
import type ProjectManager from "../ProjectManager/ProjectManager";

interface GistUploadModalsProps {
  modalRef: React.RefObject<Modal>;
  fileTreeRef: React.RefObject<typeof FileTree>;
  projectManager: ProjectManager;
}

function GistUploadModals({
  modalRef,
  fileTreeRef,
  projectManager,
}: GistUploadModalsProps): React.ReactElement {
  const [loading, setLoading] = useState(false);
  const [gistLink, setGistLink] = useState("");
  const [newToken, setNewToken] = useState("");

  /* eslint-disable */
  // @ts-ignore
  const token: string = useSelector((state) => state.gistToken);
  /* eslint-enable */

  const dispatch = useDispatch();

  const getGistId = (str: string) => {
    const idr = /[0-9A-Fa-f]{8,}/;
    const match = idr.exec(str);
    return match ? match[0] : null;
  };

  const onCreate = async () => {
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

    if (gistLink !== "") {
      setGistLink("");
      modalRef.current?.closeModal().catch((me: Error) => {
        console.error(me);
      });
      return;
    }

    setLoading(true);

    const link = await fileOps.uploadGistProject(token, root).catch((e: Error) => {
      notification.error("Gist load error", e.message);
    });

    if (!link) {
      setLoading(false);
      return;
    }

    const gistId = getGistId(link);

    if (projectManager.projectSettings) {
      await projectManager.projectSettings.set("gistId", gistId).catch((e: Error) => {
        notification.error("GistId saving error", e.message);
      });
    }

    setLoading(false);
    setGistLink(link);
  };

  return (
    <Modal
      ref={modalRef}
      title="Upload workspace to gist"
      textConfirm={gistLink === "" && newToken === "" ? "Upload" : newToken === "" ? "Ok" : "Save"}
      pending={loading && "Uploading..."}
      confirmDisabled={!token}
      onConfirm={
        newToken === ""
          ? onCreate
          : () => {
              dispatch({ type: "SET_GIST_TOKEN", payload: newToken });
              setNewToken("");
            }
      }
      onCancel={() => {
        setLoading(false);
        setNewToken("");
        setGistLink("");
        return true;
      }}
    >
      {gistLink === "" && (
        <DebouncedFormGroup
          label={
            <div>
              To upload your project you need to add github token, or leave the default one to
              create a gist without a Github account. Currenty you are going to use{" "}
              <kbd>{token}</kbd>{" "}
              {token === atob("Z2hwX3dNYkNNS2Z1MGs1d1loZzl4aDRVODBlT1BBdUpGUjF6b3Z4TA==") ? (
                <b>default</b>
              ) : (
                <b>custom</b>
              )}{" "}
              token. You can change saved token below.
            </div>
          }
          maxLength="50"
          value={newToken}
          placeholder="Token"
          onChange={(t: string) => setNewToken(t)}
        />
      )}
      {gistLink !== "" && (
        <>
          <p>
            Your Gist link{" "}
            <a href={gistLink} target="_blank" rel="noreferrer">
              {gistLink}
            </a>
          </p>
          <p>
            Its id will be stored in config. You will be able to use it to update files in gist.
          </p>
        </>
      )}
    </Modal>
  );
}

export default GistUploadModals;

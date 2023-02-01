import React, { useEffect, useRef } from "react";
import { Modal } from "~/base-components/ui-components";

interface OpenExistingProjectModallProps {
  isOpen: boolean;
  projectInfo: { path: string; gistId: string };
  callback: (action: "open" | "update" | "copy") => void;
}

const OpenExistingProjectModal: React.FC<OpenExistingProjectModallProps> = ({
  isOpen,
  projectInfo,
  callback,
}) => {
  const modalRef = useRef<Modal>(null);

  useEffect(() => {
    if (isOpen) {
      modalRef.current?.openModal().catch((me: Error) => {
        console.error(me);
      });
    } else {
      modalRef.current?.closeModal().catch((me: Error) => {
        console.error(me);
      });
    }
  }, [isOpen]);

  return (
    <Modal
      ref={modalRef}
      title="Project already exists"
      textConfirm="Copy"
      textCancel="Update"
      textAddition="Open"
      colorCancel="primary"
      colorAddition="primary"
      onConfirm={() => callback("copy")}
      onAdditionAction={() => callback("open")}
      onCancel={() => callback("update")}
    >
      <p>
        You are going to open shared project with <kbd>{projectInfo.gistId}</kbd> Gist ID. But it is
        already exists as <kbd>{projectInfo.path.replace(".workspaces/", "")}</kbd> project locally.
        You can <b>open</b> <kbd>{projectInfo.path.replace(".workspaces/", "")}</kbd> project,{" "}
        <b>update</b> it with latest data from <kbd>{projectInfo.gistId}</kbd> or create a{" "}
        <b>copy</b>.
      </p>
    </Modal>
  );
};

export default OpenExistingProjectModal;

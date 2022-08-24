import { forwardRef, useImperativeHandle, useRef, useState } from "react";
import fileOps from "~/base-components/file-ops";

import { Modal, DebouncedFormGroup } from "~/base-components/ui-components";

import notification from "~/base-components/notification";

import { ProjectManager } from "../ProjectManager";
import actions from "../actions";
import { GistContent } from "~base-components/file-ops/GistFs";

const OpenProjectModal = forwardRef((_, ref) => {
  actions.openProjectModal = ref;
  const modalRef = useRef<Modal>(null);
  const [name, setName] = useState("");
  const [link, setLink] = useState("");
  const [loading, setLoading] = useState(false);
  const resolverRef = useRef<
    ((_: { id: string; author: string; path: string; name: string }) => void) | undefined
  >(undefined);

  useImperativeHandle(ref, () => ({
    async openModal() {
      setName("");
      setLink("");
      setLoading(false);
      await modalRef.current?.openModal();
      return new Promise((resolve) => {
        resolverRef.current = resolve;
      });
    },
  }));

  const onOpenProject = async () => {
    setLoading(true);

    const gistId = getGistId(link);

    if (gistId === null) {
      return;
    }

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

    const created = await openProject(obj, name);

    if (created && resolverRef.current) {
      modalRef.current?.closeModal().catch((me: Error) => {
        console.error(me);
      });
      resolverRef.current(created);
      setName("");
      setLink("");
      setLoading(false);
    } else {
      setLoading(false);
    }
  };

  const getGistId = (str: string) => {
    const idr = /[0-9A-Fa-f]{8,}/;
    const match = idr.exec(str);
    return match ? match[0] : null;
  };

  const openProject = async (obj: GistContent, pName: string) => {
    try {
      const Manager = ProjectManager;
      const created = await Manager.openProject(obj, pName);
      notification.success("Successful", `New project <b>${pName}</b> is loaded.`);
      return created;
    } catch (e: any) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
      notification.error("Cannot Create the Project", e.message);
      return false;
    }
  };

  return (
    <Modal
      ref={modalRef}
      title="Load project from Gist"
      textConfirm="Load"
      onConfirm={onOpenProject}
      pending={loading && "Loading..."}
      confirmDisabled={!name || !link}
    >
      <DebouncedFormGroup label="Project name" value={name} onChange={(n: string) => setName(n)} />
      <DebouncedFormGroup label="Gist link" value={link} onChange={(l: string) => setLink(l)} />
    </Modal>
  );
});

export default OpenProjectModal;

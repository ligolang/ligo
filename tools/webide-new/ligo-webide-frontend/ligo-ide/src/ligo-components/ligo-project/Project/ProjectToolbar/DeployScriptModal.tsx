import React, { useState } from "react";
import { Modal, DebouncedFormGroup } from "~/base-components/ui-components";
import notification from "~/base-components/notification";
import { WebIdeApi } from "~/components/api/api";
import { CompilerManager } from "~/ligo-components/ligo-compiler";
import { validName } from "~/components/validators";

interface DeployScriptModalProps {
  modalRef: React.RefObject<Modal>;
  projectSettings: any;
  projectManager: any;
}

const DeployScriptModal = ({
  modalRef,
  projectSettings,
  projectManager,
}: DeployScriptModalProps): React.ReactElement | null => {
  const [storage, setStorage] = useState<string>("");
  const [name, setName] = useState<string>("");
  const [loading, setLoading] = useState<boolean>(false);
  const [result, setResult] = useState<string>("");
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
  const tzFilePath: string = projectSettings?.get("main") || "";

  const onCreate = async () => {
    setLoading(true);

    let contractFiles = [];

    try {
      // eslint-disable
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
      contractFiles = await projectManager.getMainContract();
    } catch (e: any) {
      if (e instanceof Error) {
        notification.error("Generate deploy script error", e.message);
      } else {
        console.error(e);
      }
      setLoading(false);
      return;
    }

    await WebIdeApi.generateDeployScript({
      name,
      project: {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
        sourceFiles: contractFiles,
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access
        main: projectManager.mainFilePath,
      },
      storage,
    })
      .then(async (resp) => {
        setResult(resp.data.script);
        // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
        await CompilerManager.saveCompiledContract(resp.data.build, projectManager);
      })
      .catch((e: Error) => {
        modalRef.current?.closeModal().catch((me: Error) => {
          console.error(me);
        });
        notification.error("Deploy Script Error", e.message);
      });
    setLoading(false);
  };

  return (
    <Modal
      ref={modalRef}
      title="Deploy Script"
      textConfirm="Generate"
      pending={loading && "Generating"}
      confirmDisabled={storage === "" || name === "" || (name !== "" && !!validName(name))}
      onConfirm={onCreate}
      onCancel={() => {
        setLoading(false);
        setName("");
        setStorage("");
        setResult("");
        return true;
      }}
    >
      <DebouncedFormGroup
        label={
          <div>
            Generate deploy script for <kbd>{tzFilePath}</kbd> with init storage (ligo expression)
          </div>
        }
        value={storage}
        type="textarea"
        onChange={(st: string) => setStorage(st)}
        placeholder="Storage"
      />
      <DebouncedFormGroup
        label={<div>Contract name in deploy script</div>}
        value={name}
        onChange={(n: string) => setName(n)}
        placeholder="Name"
        validator={validName}
      />
      {result && (
        <>
          <div>Result</div>
          <pre className="pre-box pre-wrap break-all bg-primary text-body">{result}</pre>
        </>
      )}
    </Modal>
  );
};

export default DeployScriptModal;

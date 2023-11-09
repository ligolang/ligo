import React, { useEffect, useState } from "react";
import { Modal, DebouncedFormGroup, DropdownInput } from "~/base-components/ui-components";
import notification from "~/base-components/notification";
import { WebIdeApi } from "~/components/api/api";

interface ExpressionManagerModalProps {
  modalRef: React.RefObject<Modal>;
  currentTab: string;
  isOpen: boolean;
  close: () => void;
  managerType: "dryRun" | "compile";
  projectManager: any;
}

const ExpressionManagerModal = ({
  modalRef,
  currentTab,
  isOpen,
  close,
  managerType,
  projectManager,
}: ExpressionManagerModalProps): React.ReactElement | null => {
  const [storage, setStorage] = useState<string>("");
  const [params, setParams] = useState<string>("");
  const [name, setName] = useState<string>("");
  const [loading, setLoading] = useState<boolean>(false);
  const [result, setResult] = useState<string>("");
  const [files, setFiles] = useState<any[]>([]);
  const [declarations, setDeclarations] = useState<{ id: string; display: string }[]>([]);

  useEffect(() => {
    const processFile = async () => {
      notification.info("Loading declarations");

      let contractFiles = [];

      try {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
        contractFiles = await projectManager.getMainContract();
      } catch (e: any) {
        if (e instanceof Error) {
          notification.error("Generate deploy script error", e.message);
        } else {
          console.error(e);
        }
        return;
      }

      // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
      setFiles(contractFiles);

      const decls = await WebIdeApi.listDeclarations({
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access
        project: { sourceFiles: contractFiles, main: projectManager.mainFilePath },
        onlyEndpoint: managerType === "dryRun",
      });
      setDeclarations(
        decls.data.map((d) => {
          return { id: d, display: d };
        })
      );
      if (modalRef.current) {
        await modalRef.current.openModal();
      }
    };

    if (isOpen) {
      processFile().catch((e: Error) => {
        modalRef.current?.closeModal().catch((me: Error) => {
          console.error(me);
        });
        notification.error(
          managerType === "dryRun" ? "Dry Run Error" : "Compile Expression Error",
          e.message
        );
      });
    }
  }, [currentTab, isOpen, managerType, modalRef, projectManager]);

  const extractEntrypoint = (input: string): string => {
    const regex = /[^.]+$/;
    const match = regex.exec(input);
    return match ? match[0] : input;
  };

  function capitalizeFirstLetter(input: string): string {
    if (!input) {
      return input;
    }
    return input.charAt(0).toUpperCase() + input.slice(1).toLowerCase();
  }

  const generateEntrypointParameter = (entrypoint: string, parameter: string): string =>
    `${capitalizeFirstLetter(entrypoint)}(${parameter})`;

  const extractModule = (input: string): string => {
    if (!input.includes(".")) return "";
    const regex = /^[^.]+/;
    const match = regex.exec(input);
    return match ? match[0] : input;
  };

  const onCreate = async () => {
    setLoading(true);

    const entrypoint = extractEntrypoint(name);
    const parameterIncludingVariant = generateEntrypointParameter(entrypoint, params);
    const module = extractModule(name);

    await (managerType === "dryRun"
      ? WebIdeApi.dryRun({
          // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access
          project: { sourceFiles: files, main: projectManager.mainFilePath, module },
          storage,
          parameters: parameterIncludingVariant,
        })
      : WebIdeApi.compileExpression({
          function: name,
          // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access
          project: { sourceFiles: files, main: projectManager.mainFilePath },
        })
    )
      .then((resp) => setResult(resp.data))
      .catch((e: Error) => {
        notification.error(
          managerType === "dryRun" ? "Dry Run Error" : "Compile Expression Error",
          e.message
        );
      });

    setLoading(false);
  };

  return (
    <Modal
      ref={modalRef}
      title={managerType === "dryRun" ? "Dry Run" : "Compile Expression"}
      textConfirm={managerType === "dryRun" ? "Run" : "Compile"}
      pending={loading && (managerType === "dryRun" ? "Running" : "Compiling")}
      confirmDisabled={
        name === "" || (managerType === "dryRun" ? storage === "" || params === "" : false)
      }
      onConfirm={onCreate}
      onCancel={() => {
        setLoading(false);
        setName("");
        setStorage("");
        setParams("");
        setResult("");
        setFiles([]);
        setDeclarations([]);
        close();
        return true;
      }}
    >
      <DropdownInput
        label={
          <div>
            Access function for <kbd>{currentTab}</kbd>
          </div>
        }
        options={declarations}
        placeholder="Please select a function"
        value={name}
        onChange={(template: string) => setName(template)}
        size={managerType === "dryRun" ? "" : "short"}
      />
      {managerType === "dryRun" && (
        <>
          <DebouncedFormGroup
            label={<div>Input parameters (ligo expression)</div>}
            value={params}
            onChange={(n: string) => setParams(n)}
            placeholder="Parameters"
            type="textarea"
          />
          <DebouncedFormGroup
            label={<div>Storage (ligo expression)</div>}
            value={storage}
            onChange={(st: string) => setStorage(st)}
            placeholder="Storage"
            type="textarea"
          />
        </>
      )}
      {result && (
        <>
          <div>Result</div>
          <pre className="pre-box pre-wrap break-all bg-primary text-body">{result}</pre>
        </>
      )}
    </Modal>
  );
};

export default ExpressionManagerModal;

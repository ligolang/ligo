import React, { useState } from "react";
import { Input, Label, Modal } from "~/base-components/ui-components";

interface CompileModalProps {
  modalRef: React.RefObject<Modal>;
  tzFilePath: string;
  mainFilePath: string;
  onCompile: any;
  isPreDeploy: boolean;
}

const CompileModal: React.FC<CompileModalProps> = ({
  modalRef,
  tzFilePath,
  mainFilePath,
  onCompile,
  isPreDeploy,
}) => {
  const [doNotShow, setDoNotShow] = useState(false);
  const onCreate = async () => {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-call
    onCompile(doNotShow);
    setDoNotShow(false);
    await modalRef.current?.closeModal();
  };

  return (
    <Modal
      ref={modalRef}
      title={isPreDeploy ? "Contract is not compiled" : "Compile"}
      textConfirm="Compile"
      onConfirm={onCreate}
    >
      {isPreDeploy ? (
        <div>
          You are going to deploy <kbd>{tzFilePath}</kbd> contract but it doesn&apos;t exist. Your
          main file is <kbd>{mainFilePath}</kbd>. Please check config or compile main file to
          deploy.
        </div>
      ) : (
        <>
          <div>
            You are going to compile <kbd>{mainFilePath}</kbd> contract. If you want to use another
            contract, please change it in config.
          </div>
          <br />
          <div className="ml-4">
            <Input
              type="checkbox"
              disabled={false}
              onChange={() => setDoNotShow(!doNotShow)}
              checked={doNotShow}
            />
            <Label>
              Do not show this again. You can use <kbd>ctrl/cmd+b</kbd> shortcut to compile.
            </Label>
          </div>
        </>
      )}
    </Modal>
  );
};

export default CompileModal;

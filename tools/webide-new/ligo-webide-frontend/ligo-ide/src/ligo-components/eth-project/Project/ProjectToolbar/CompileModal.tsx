import React from "react";
import { Modal } from "~/base-components/ui-components";

interface CompileModalProps {
  modalRef: React.RefObject<Modal>;
  tzFilePath: string;
  onCompile: any;
}

const CompileModal: React.FC<CompileModalProps> = ({ modalRef, tzFilePath, onCompile }) => {
  const onCreate = async () => {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-call
    onCompile();
    await modalRef.current?.closeModal();
  };

  return (
    <Modal ref={modalRef} title="Compile" textConfirm="Compile" onConfirm={onCreate}>
      <div>
        You are going to compile <kbd>{tzFilePath}</kbd> contract. If you want to use another
        contract, please change it in config.
      </div>
    </Modal>
  );
};

export default CompileModal;

import React, { useState } from "react";
import { Modal } from "~/base-components/ui-components";

interface RunTestModalProps {
  modalRef: React.RefObject<Modal>;
  filePathToTest: string;
  onTest: any;
}

const RunTestModal: React.FC<RunTestModalProps> = ({ modalRef, filePathToTest, onTest }) => {
  const [loading, setLoading] = useState<boolean>(false);

  const onConfirm = async () => {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-call
    onTest(filePathToTest);
    await modalRef.current?.closeModal();
  };

  return (
    <Modal
      ref={modalRef}
      title="Run tests"
      textConfirm="Run test"
      pending={loading}
      onConfirm={onConfirm}
      onCancel={() => {
        setLoading(false);
        return true;
      }}
    >
      <div>
        You are going to run tests of file <kbd>{filePathToTest}</kbd>
      </div>
    </Modal>
  );
};

export default RunTestModal;

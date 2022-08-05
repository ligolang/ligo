import React, { useRef, forwardRef } from "react";
import PropTypes from "prop-types";
import { Modal, Button, ListGroupItem } from "~/base-components/ui-components";

const TutorialModal = forwardRef(({ header, description, nextPage }, ref) => {
  const modal = useRef(null);

  if (ref) {
    ref.current = {
      showModal: () => {
        showModal();
      },
      closeModal: () => {
        closeModal();
      },
    };
  }

  const showModal = () => {
    modal.current.openModal();
  };

  const closeModal = () => {
    modal.current.closeModal();
  };

  const toGuidePage = () => {
    window.open(nextPage, "_blank");
    closeModal();
  };

  return (
    <Modal ref={modal} title={header}>
      <div>
        <p style={{ fontSize: "15px" }}>{description}</p>
        <ListGroupItem
          className="center"
          style={{
            margin: "5px 0",
            borderRadius: "6px",
          }}
        >
          <div className="center">
            <div className="tutorialPanel" />
            <p>Learn how to use Black IDE</p>
          </div>

          <Button onClick={toGuidePage} color="primary">
            Open
          </Button>
        </ListGroupItem>
      </div>
    </Modal>
  );
});

TutorialModal.propTypes = {
  header: PropTypes.string,
  description: PropTypes.string,
  nextPage: PropTypes.string,
};

export default TutorialModal;

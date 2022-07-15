import React from "react";

export default function ({ children }) {
  return (
    <div className="d-flex h-100 flex-column">
      <div className="jumbotron jumbotron-fluid h-100 m-0">
        <div className="container">{children}</div>
      </div>
    </div>
  );
}

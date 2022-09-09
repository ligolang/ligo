import React from "react";

export default function Card({ title, right, children, noPadding }) {
  if (typeof title === "string") {
    title = <div className="h4 mb-0">{title}</div>;
  }
  return (
    <div className={`card card-body h-100 border-0 ${noPadding ? "p-0" : ""}`}>
      <div className="card-title d-flex flex-row justify-content-between align-items-start mb-2">
        {title}
        <div className="d-flex flex-row align-items-center">{right}</div>
      </div>
      {children}
    </div>
  );
}

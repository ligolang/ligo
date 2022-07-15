import React from "react";
import PropTypes from "prop-types";

const TYPE_COLOR = {
  error: "#D1606C",
  warning: "#D1AD60",
  // unsaved: '#FFF'
};

const StatusTitle = ({ title, isLeaf, showType, count }) => {
  const typeColor = TYPE_COLOR[showType];
  return (
    <div className="status-title" style={{ color: `${typeColor}` }}>
      <p className="status-title__title"> {title}</p>
      {isLeaf ? (
        <p className="status-title__count">{count}</p>
      ) : (
        <div className="status-title__circleBadge" style={{ backgroundColor: `${typeColor}` }} />
      )}
    </div>
  );
};

export default StatusTitle;

StatusTitle.propTypes = {
  isLeaf: PropTypes.bool,
  title: PropTypes.string,
  showType: PropTypes.string,
  count: PropTypes.number,
};

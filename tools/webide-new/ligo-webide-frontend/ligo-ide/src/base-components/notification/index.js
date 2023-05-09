import React from "react";

import ReactNotificationSystem from "react-notification-system";
import manager from "./manager";
import "./style.css";

export function NotificationSystem() {
  return <ReactNotificationSystem ref={(ref) => (manager.ref = ref)} allowHTML />;
}

export default manager;

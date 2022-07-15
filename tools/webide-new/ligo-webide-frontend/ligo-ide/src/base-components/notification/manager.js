class Notification {
  constructor(ref, { title, message, level = "success", autoDismiss = 2, action = undefined }) {
    this.ref = ref;
    if (this.ref) {
      setTimeout(() => {
        this.notification = this.ref.addNotification({
          title,
          message,
          level,
          position: "tc",
          autoDismiss,
          dismissible: false,
          action,
        });
      }, 0);
    }
  }

  dismiss() {
    if (this.ref && this.notification) {
      this.ref.removeNotification(this.notification);
    }
  }

  modify(title, message, level) {
    if (this.ref && this.notification) {
      this.ref.editNotification(this.notification, {
        title,
        message,
        level,
      });
    }
  }
}

class NotificationManager {
  constructor() {
    this._ref = null;
  }

  set ref(ref) {
    this._ref = ref;
  }

  displayNotification(opts) {
    return new Notification(this._ref, opts);
  }

  success(title, message, autoDismiss = 2, actions = undefined) {
    return this.displayNotification({
      title,
      message,
      level: "success",
      autoDismiss,
      actions,
    });
  }

  error(title, message, autoDismiss = 4, actions = undefined) {
    return this.displayNotification({
      title,
      message,
      level: "error",
      autoDismiss,
      actions,
    });
  }

  warning(title, message, autoDismiss = 2, actions = undefined) {
    return this.displayNotification({
      title,
      message,
      level: "warning",
      autoDismiss,
      actions,
    });
  }

  info(title, message, autoDismiss = 2, actions = undefined) {
    return this.displayNotification({
      title,
      message,
      level: "info",
      autoDismiss,
      actions,
    });
  }
}

export default new NotificationManager();

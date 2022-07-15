class GlobalModalManager {
  constructor() {
    this.autoUpdateModal = null;
    this.aboutModal = null;
  }

  openAutoUpdateModal(version) {
    if (this.autoUpdateModal) {
      return this.autoUpdateModal.openModal(version);
    }
  }

  openAboutModal() {
    if (this.aboutModal) {
      this.aboutModal.openModal();
    }
  }
}

export default new GlobalModalManager();

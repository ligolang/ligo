import set from "lodash/set";
import get from "lodash/get";
import fileOps from "~/base-components/file-ops";

export default class ProjectSettings {
  static configFileName = "config.json";

  constructor(projectManager, settingFilePath, channel) {
    this.projectManager = projectManager;
    this.settingFilePath = settingFilePath;
    this.channel = channel;
    channel.off("current-value");
    channel.on("current-value", (evt) => this.triggerEvent(evt));

    this.invalid = false;
    this.settings = {};
  }

  async readSettings() {
    let settingsJson;
    try {
      settingsJson = await fileOps.readFile(this.settingFilePath);
    } catch (e) {}

    this.update(settingsJson);
    return this.settings;
  }

  async writeSettings(rawSettings) {
    const settings = this.trimSettings(rawSettings);

    const settingsJson = JSON.stringify(settings, null, 2);
    await fileOps.writeFile(this.settingFilePath, settingsJson);
  }

  update(settingsJson) {
    let rawSettings;
    try {
      rawSettings = JSON.parse(settingsJson || "{}");
    } catch (e) {
      return;
    }
    const oldSettings = this.settings;
    this.settings = this.trimSettings(rawSettings);

    if (!this.channel) {
      return;
    }

    this.channel.events.forEach((evt) => this.triggerEvent(evt, oldSettings));
  }

  triggerEvent(evt, oldSettings) {
    const [prefix, key] = evt.split(":");
    if (prefix !== "settings" || !key) {
      return;
    }

    const value = get(this.settings, key);
    if (oldSettings) {
      const oldValue = get(oldSettings, key);
      if (oldValue === value) {
        return;
      }
    }
    this.channel.trigger(evt, value);
  }

  trimSettings = (rawSettings = {}) => rawSettings;

  get(key) {
    return get(this.settings, key);
  }

  async set(key, value) {
    const { settings } = this;
    const oldValue = get(settings, key);
    if (oldValue !== value) {
      set(settings, key, value);
      this.channel.trigger(`settings:${key}`, value);
      await this.writeSettings(settings);
    }

    if (key === "compilers.solc") {
      this.projectManager.lint();
    }
  }

  trimSettings = (rawSettings = {}) => {
    const compilers = rawSettings.compilers || {};
    const settings = {
      main: rawSettings.main || "./contracts/Contract.sol",
      deploy: rawSettings.deploy,
      framework: rawSettings.framework || `${process.env.COMPILER_VERSION_KEY}-docker`,
      npmClient: rawSettings.npmClient,
      compilers: {
        ...compilers,
        [process.env.COMPILER_VERSION_KEY]: compilers[process.env.COMPILER_VERSION_KEY] || "",
        solc: compilers.solc || "",
        evmVersion: compilers.evmVersion || "istanbul",
        optimizer: compilers.optimizer,
      },
      linter: rawSettings.linter || "solhint",
      editor: {
        fontFamily: rawSettings.editor?.fontFamily || "Hack",
        fontSize: rawSettings.editor?.fontSize || "13px",
        ligatures: Boolean(rawSettings.editor?.ligatures),
      },
    };
    if (rawSettings.language) {
      settings.language = rawSettings.language;
    }
    if (!settings.npmClient) {
      delete settings.npmClient;
    }
    return settings;
  };
}

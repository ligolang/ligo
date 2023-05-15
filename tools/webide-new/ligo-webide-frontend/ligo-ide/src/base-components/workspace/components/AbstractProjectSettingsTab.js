import React, { PureComponent } from "react";

import debounce from "lodash/debounce";
import { Modal, DebouncedFormGroup, Button } from "~/base-components/ui-components";

import actions from "../actions";

export default class AbstractProjectSettingsTab extends PureComponent {
  onChangeHandlers = {};

  debouncedUpdate = debounce(() => this.forceUpdate(), 500, {
    leading: true,
    trailing: false,
  }).bind(this);

  onChange = (key) => {
    if (!this.onChangeHandlers[key]) {
      this.onChangeHandlers[key] = (value) => {
        this.context.projectSettings?.set(key, value);
      };
    }
    return this.onChangeHandlers[key];
  };
}

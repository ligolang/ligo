import React, { PureComponent } from "react";

import { DropdownInput, Badge } from "~/base-components/ui-components";

import notification from "~/base-components/notification";
import keypairManager from "./keypairManager";
import { utils } from "~/ligo-components/eth-sdk";

export default class KeypairInputSelector extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      keypairs: [],
      options: [],
      extraOptions: [],
    };
    this.initKeyPair();
    const { networkManager } = require("~/ligo-components/eth-network");
    this.networkManager = networkManager;
    this.abbriviFunc = this.abbriviFunc.bind(this);
    this.findPlaceholder = this.findPlaceholder.bind(this);
    this.findExtraOptions = this.findExtraOptions.bind(this);
  }

  initKeyPair() {
    keypairManager.onUpdated(this.updateKeypairs);
    keypairManager.loadAndUpdateKeypairs();
  }

  componentDidUpdate(prevProps) {
    prevProps.filter !== this.props.filter && this.updateKeypairs(this.allKeypairs || []);
  }

  updateKeypairs = (allKeypairs) => {
    const { extraOptions, extraAdrress } = this.findExtraOptions();
    this.allKeypairs = allKeypairs;
    const keypairs = this.props.filter ? allKeypairs.filter(this.props.filter) : allKeypairs;
    const AllAddress = Array.from(new Set(keypairs.map((el) => el.address).concat(extraAdrress)));
    if (!this.props.editable) {
      if (this.state.keypairs.length && !keypairs.length) {
        this.props.onChange();
      }
      if (keypairs.length && !AllAddress.includes(this.props.value)) {
        this.props.onChange(keypairs[0].address);
      }
    }
    this.setState({
      extraOptions,
      keypairs,
      options: keypairs.map(this.mapKeyToOption),
    });
  };

  abbriviFunc(address) {
    return `${utils.isValidAddressReturn(address).substr(0, 6)}...${utils
      .isValidAddressReturn(address)
      .substr(-6)}`;
  }

  findExtraOptions() {
    let extraAdrress = [];
    const extraOptions = this.props.extra
      ? // eslint-disable-next-line array-callback-return
        this.props.extra.map((item) => {
          if (item.children) {
            item.children.forEach((ele) => {
              ele.address && extraAdrress.push(ele.address);
            });
            return {
              ...item,
              children: item.children.map(this.mapKeyToOption),
            };
          }
        })
      : [];

    return {
      extraAdrress,
      extraOptions,
    };
  }

  findPlaceholder() {
    const { placeholder, editable } = this.props;
    const { options, extraOptions } = this.state;
    if (!placeholder) {
      if (options.length || extraOptions.length) {
        return editable ? "Select or type an address" : "Select an address";
      }
      return "(No keys in keypair manager)";
    }
  }

  renderDisplay = (key) => {
    const { name } = key;
    const abbreviationOption = this.props.abbreviationOption;
    const address = utils.formatAddress(key.address, this.networkManager?.current?.chainId);
    return (highlight, active) => {
      let highlightAddress = address;
      if (!active && highlight) {
        highlightAddress = [];
        let reg = new RegExp(highlight, "ig");
        let tempArr = address.replaceAll(reg, (text) => `,spc-${text},`);
        tempArr.split(",").forEach((part) => {
          if (part === "") return;
          if (part.indexOf("spc") !== -1) {
            let splitAddress = part.split("spc-")[1];
            splitAddress = abbreviationOption ? this.abbriviFunc(splitAddress) : splitAddress;
            highlightAddress.push(<b className="text-primary">{splitAddress}</b>);
          } else {
            highlightAddress.push(part);
          }
        });
      }

      if (abbreviationOption) {
        if (highlightAddress && typeof highlightAddress[0] === "string") {
          const validValue = Array.isArray(highlightAddress)
            ? highlightAddress[0]
            : highlightAddress;
          highlightAddress = this.abbriviFunc(validValue);
        }
      }

      return (
        <div className="w-100 d-flex align-items-center justify-content-between">
          <code className="text-overflow-dots mr-1">{highlightAddress}</code>
          <Badge color="info" style={{ top: 0 }}>
            {name}
          </Badge>
        </div>
      );
    };
  };

  mapKeyToOption = (key) => {
    return {
      id: key.address,
      badge: key.name,
      display: this.renderDisplay(key),
    };
  };

  render() {
    const {
      size,
      label,
      editable,
      maxLength,
      icon = "fas fa-key",
      noCaret,
      value,
      onChange,
      extra = [],
      abbreviationOption = false,
      invalid,
    } = this.props;

    const { options, extraOptions } = this.state;

    const onClick = () => {
      if (!editable && !options.length && !extraOptions.length) {
        notification.error(
          "No Available Keypiar",
          "Please create or import a keypair in the keypair manager first."
        );
      }
    };

    return (
      <DropdownInput
        size={size}
        label={label}
        placeholder={this.findPlaceholder()}
        editable={editable}
        maxLength={maxLength}
        inputClassName={value ? "code" : ""}
        addon={
          <span key={`key-icon-${icon.replace(/\s/g, "-")}`}>
            <i className={icon} />
          </span>
        }
        noCaret={typeof noCaret === "boolean" ? noCaret : size === "sm"}
        options={[...this.state.options, ...this.state.extraOptions]}
        renderText={
          !editable &&
          ((option) => (option ? <code>{utils.isValidAddressReturn(option.id)}</code> : null))
        }
        value={value}
        onChange={onChange}
        invalid={invalid}
        onClick={onClick}
      />
    );
  }
}

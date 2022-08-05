import React, { PureComponent } from "react";

import { withRouter } from "react-router-dom";
import Highlight from "react-highlight";
import { ButtonOptions, Table, TableCardRow, Badge } from "~/base-components/ui-components";

import { networkManager } from "~/ligo-components/eth-network";
import { ResultContent } from "~/ligo-components/eth-contract";

class TransactionDetails extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      selected: "basic",
      format: "pretty",
    };
    this.modal = React.createRef();
  }

  renderContent = () => {
    const { tx = {}, closeModal, history } = this.props;
    const { selected, format } = this.state;
    const { txHash, status, data = {} } = tx;
    const {
      functionName,
      contractName,
      signer,
      params,
      value,
      confirmed,
      transaction,
      result,
      error,
      receipt,
      abi,
    } = data;
    const contractAddress = receipt?.contractAddress || data.contractAddress;

    if (selected === "basic") {
      return (
        <Table>
          <TableCardRow name="Hash" icon="fas fa-hashtag" badge={<code>{txHash}</code>} />
          <TableCardRow
            name="Status"
            icon="fas fa-circle-notch"
            badge={status === "FAILED-TIMEOUT" ? "TIMEOUT" : status}
            badgeColor={
              status.startsWith("FAILED")
                ? "danger"
                : status === "CONFIRMED"
                ? "success"
                : "warning"
            }
          />
          {this.renderError(error)}
          {contractAddress && (
            <TableCardRow
              name="Contract"
              icon="fas fa-file-invoice"
              badge={
                <a
                  href="javascript:void(0)"
                  onClick={() => {
                    history.push(`/contract/${contractAddress}`);
                    this.props.closeModal();
                  }}
                  className="text-body"
                >
                  <code>{contractAddress}</code>
                </a>
              }
            />
          )}
          {functionName && (
            <TableCardRow name="Function" icon="fas fa-calculator" badge={functionName} />
          )}
          {contractName && (
            <TableCardRow name="Contract Name" icon="fas fa-file-invoice" badge={contractName} />
          )}
          {value && (
            <TableCardRow
              name={`${networkManager.symbol} Sent`}
              icon="fas fa-coins"
              badge={`${networkManager.sdk?.utils.unit.fromValue(value)} ${networkManager.symbol}`}
            />
          )}
          {confirmed && <TableCardRow name="Result" icon="fas fa-sign-out-alt" badge={confirmed} />}
          <TableCardRow
            name="Signer"
            icon="fas fa-key"
            badge={
              <a
                href="javascript:void(0)"
                onClick={() => {
                  this.props.closeModal();
                  history.push(`/account/${signer}`);
                }}
                className="text-body"
              >
                <code>{signer}</code>
              </a>
            }
          />
          {receipt && receipt.contractCreated && (
            <TableCardRow
              name="Contract Created"
              icon="fas fa-file-invoice"
              badge={
                <a
                  href="javascript:void(0)"
                  onClick={() => {
                    history.push(`/contract/${receipt.contractCreated}`);
                    this.props.closeModal();
                  }}
                  className="text-body"
                >
                  <code>{receipt.contractCreated}</code>
                </a>
              }
            />
          )}
        </Table>
      );
    }
    if (selected === "params") {
      return (
        <Highlight
          language="javascript"
          className="pre-box bg2 pre-wrap break-all small my-0"
          element="pre"
        >
          <code>{JSON.stringify(params, null, 2)}</code>
        </Highlight>
      );
    }
    if (selected === "tx") {
      return (
        <Highlight
          language="javascript"
          className="pre-box bg2 pre-wrap break-all small my-0"
          element="pre"
        >
          <code>{JSON.stringify(transaction, null, 2)}</code>
        </Highlight>
      );
    }
    if (selected === "receipt") {
      return (
        <Highlight
          language="javascript"
          className="pre-box bg2 pre-wrap break-all small my-0"
          element="pre"
        >
          <code>{JSON.stringify(receipt, null, 2)}</code>
        </Highlight>
      );
    }
    if (selected === "result") {
      return (
        <>
          <div>
            <ButtonOptions
              size="sm"
              options={[
                { key: "pretty", text: "Pretty" },
                { key: "raw", text: "Raw" },
              ]}
              selected={format}
              onSelect={(format) => this.setState({ format })}
            />
          </div>
          <ResultContent format={format} actionResult={result} onNavigate={closeModal} />
        </>
      );
    }
    if (selected === "error") {
      return (
        <Highlight
          language="javascript"
          className="pre-box bg2 pre-wrap break-all small my-0"
          element="pre"
        >
          <code>{JSON.stringify(error, null, 2)}</code>
        </Highlight>
      );
    }
    if (selected === "abi") {
      return (
        <Highlight
          language="javascript"
          className="pre-box bg2 pre-wrap break-all small my-0"
          element="pre"
        >
          <code>{JSON.stringify(abi, null, 2)}</code>
        </Highlight>
      );
    }
  };

  renderError = (error) => {
    if (!error) {
      return null;
    }

    const { code = "", message = "", data = "" } = error;

    return (
      <TableCardRow
        name="Error"
        icon="fas fa-exclamation-triangle"
        badge={code}
        badgeColor="danger"
      >
        <div className="mt-2">{message}</div>
        {data && (
          <div className="mt-1">
            <Badge color="secondary" className="mr-1 p-relative" style={{ top: -1 }}>
              data
            </Badge>
            {data}
          </div>
        )}
      </TableCardRow>
    );
  };

  render() {
    const tx = this.props.tx || {};
    const { selected } = this.state;

    const options = [
      { key: "basic", text: "Basic" },
      { key: "params", text: "Parameters" },
    ];
    if (tx.data?.transaction) {
      options.push({ key: "tx", text: "Tx" });
    }
    if (tx.data?.receipt) {
      options.push({ key: "receipt", text: "Receipt" });
    }
    if (tx.data?.result) {
      options.push({ key: "result", text: "Result" });
    }
    if (tx.data?.error) {
      options.push({ key: "error", text: "Error" });
    }
    if (tx.data?.abi) {
      options.push({ key: "abi", text: "ABI" });
    }

    return (
      <>
        <div>
          <ButtonOptions
            size="sm"
            className="mb-3"
            options={options}
            selected={selected}
            onSelect={(selected) => this.setState({ selected })}
          />
        </div>
        {this.renderContent()}
      </>
    );
  }
}

export default withRouter(TransactionDetails);

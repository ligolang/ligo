import React, { PureComponent } from "react";

import moment from "moment";
import { TableCard, TableCardRow } from "~/base-components/ui-components";

import networkManager from "../networkManager";

export default class RemoteNetworkInfo extends PureComponent {
  render() {
    const { networkId, url, EditButton, status } = this.props;

    return (
      <div className="d-flex">
        <div className="col-6 p-0 border-right-black">
          <TableCard title={networkManager.current?.fullName} right={EditButton}>
            {networkManager.current?.group !== "Others" && networkManager.current?.symbol && (
              <TableCardRow name="Native Coin" badge={networkManager.current?.symbol} />
            )}
            {url && <TableCardRow name="Node URL" badge={url} badgeColor="primary" />}
            {status?.chain_id && <TableCardRow name="Chain ID" badge={status?.chain_id} />}
          </TableCard>
        </div>
        <div className="col-6 p-0">
          <TableCard title="Blocks">
            {networkId && (
              <TableCardRow
                loading={!status?.header?.level}
                name="Block Number"
                badge={status?.header?.level}
              />
            )}
            {networkId && (
              <TableCardRow
                loading={!status?.header?.timestamp}
                name="Block Time"
                badge={
                  status?.header?.timestamp &&
                  moment(status.header?.timestamp).format("MMMM Do, HH:mm:ss")
                }
              />
            )}
          </TableCard>
        </div>
      </div>
    );
  }
}

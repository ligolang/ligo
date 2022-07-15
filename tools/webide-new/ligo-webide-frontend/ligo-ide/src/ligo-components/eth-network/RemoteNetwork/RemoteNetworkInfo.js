import React, { PureComponent } from "react"

import moment from 'moment'
import {
  TableCard,
  TableCardRow,
} from '~/base-components/ui-components'

import networkManager from "../networkManager"

export default class RemoteNetworkInfo extends PureComponent {
  render() {
    const { networkId, url, EditButton, info, status } = this.props;

    return (
      <div className="d-flex">
        <div className="col-6 p-0 border-right-black">
          <TableCard
            title={networkManager.current?.fullName}
            right={EditButton}
          >
            <TableCardRow name="Node URL" badge={url} badgeColor="primary" />
            {info?.chainId && (
              <TableCardRow name="Chain ID" badge={info?.chainId} />
            )}
            {info?.ensAddress && (
              <TableCardRow name="ENS" badge={info?.ensAddress} />
            )}
          </TableCard>
        </div>
        <div className="col-6 p-0">
          <TableCard title="Blocks">
            {status?.number && (
              <TableCardRow name="Block Number" badge={status?.number} />
            )}
            {status?.timestamp && (
              <TableCardRow
                name="Block Time"
                badge={moment(status.timestamp * 1000).format("MMMM Do, HH:mm:ss")}
                )}
              />
            )}
            {Boolean(status?.difficulty) && (
              <TableCardRow
                name="Difficulty"
                badge={status && Number(status.difficulty).toFixed(0)}
              />
            )}
          </TableCard>
        </div>
      </div>
    );
  }
}

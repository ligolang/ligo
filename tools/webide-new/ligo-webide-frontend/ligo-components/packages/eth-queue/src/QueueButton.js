import { QueueButton } from '@obsidians/queue'

import QueueItem from './QueueItem'
import TransactionDetails from './TransactionDetails'

QueueButton.defaultProps = {
  QueueItem,
  TransactionDetails,
}

export default QueueButton
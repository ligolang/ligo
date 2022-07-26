import React from 'react'

import notification from '@obsidians/notification'
import queue from '@obsidians/eth-queue'
import { networkManager } from '@obsidians/eth-network'

import AbiActionForm from './components/AbiActionForm'
import ResultContent from './ResultContent'

export default class ContractActions extends AbiActionForm {
  static defaultProps = {
    toolbarId: 'contract-execute-write',
    FormSection: AbiActionForm.MarginlessFormSection,
    selectorHeader: 'write functions',
    selectorIcon: 'fas fa-calculator',
    signerSelector: true,
    showResult: true,
  }

  estimate = async actionName => {
    if (!this.state.signer) {
      notification.error('Error', 'No signer is provided. Please make sure you have availabe keypairs to use in the keypair manager.')
      return
    }

    let parameters = { array: [], obj: {} }
    try {
      parameters = this.form.current.getParameters()
    } catch (e) {
      notification.error('Error in Parameters', e.message)
      return
    }

    let value
    try {
      value = networkManager.sdk.utils.unit.toValue(this.state.amount || '0')
    } catch {
      notification.error('Estimate Failed', `${networkManager.symbol} to send is invalid.`)
      return
    }

    let result
    try {
      const tx = await this.props.contract.execute(actionName, parameters, {
        from: this.state.signer,
        value,
      })
      result = await networkManager.sdk.estimate(tx)
    } catch (e) {
      notification.error('Estimate Failed', e.reason || e.message)
      return
    }

    if (result) {
      this.setState(result)
    }
  }

  executeAction = async actionName => {
    if (this.state.executing) {
      return
    }

    if (!this.state.signer) {
      notification.error('Error', 'No signer is provided. Please make sure you have availabe keypairs to use in the keypair manager.')
      return
    }

    let parameters = { array: [], obj: {} }
    try {
      parameters = this.form.current.getParameters()
    } catch (e) {
      notification.error('Error in Parameters', e.message)
      return
    }

    if (parameters.empty && parameters.array.length && !this.confirming) {
      this.confirming = true
      setTimeout(() => { this.confirming = false }, 3000)
      notification.info('Send transaction with empty parameters?', 'Press the execute button again to confirm.', 3)
      return
    }

    this.setState({ executing: true, actionError: '', actionResult: '' })

    const signer = this.state.signer

    const options = {}
    networkManager.sdk.utils.txOptions?.list.forEach(opt => {
      options[opt.name] = this.state[opt.name] || opt.default || undefined
    })

    let value
    try {
      value = networkManager.sdk.utils.unit.toValue(this.state.amount || '0')
    } catch {
      notification.error('Execute Contract Failed', `${networkManager.symbol} to send is invalid.`)
      this.setState({ executing: false })
      return
    }

    try {
      const tx = await this.props.contract.execute(actionName, parameters, {
        from: signer,
        value,
        ...options,
      })
      await queue.add(
        () => networkManager.sdk.sendTransaction(tx),
        {
          title: 'Call a Contract',
          name: actionName,
          contractAddress: this.props.contract.address,
          functionName: actionName,
          signer,
          params: parameters.obj,
          value,
          ...options,
        },
        {
          mined: res => this.setState({ executing: false, actionResult: res.result }),
          failed: ({ error }) => this.setState({ executing: false, actionError: error.message }),
        }
      )
    } catch (e) {
      console.warn(e)
      notification.error('Execute Contract Failed', e.reason || e.message)
      this.setState({ executing: false, actionError: e.reason || e.message, actionResult: '' })
    }
  }

  renderResultContent = () => <ResultContent {...this.state} />
}

import React from 'react'

import notification from '@obsidians/notification'

import AbiActionForm from './components/AbiActionForm'
import ResultContent from './ResultContent'

export default class ContractViews extends AbiActionForm {
  static defaultProps = {
    toolbarId: 'contract-execute-view',
    FormSection: AbiActionForm.MarginlessFormSection,
    selectorHeader: 'view functions',
    selectorIcon: 'far fa-eye',
    noGasOptions: true,
    showResult: true,
  }

  executeAction = async actionName => {
    if (this.state.executing) {
      return
    }

    let parameters = { array: [], obj: {} }
    try {
      parameters = this.form.current.getParameters()
    } catch (e) {
      notification.error('Error in Parameters', e.message)
      return
    }

    this.setState({ executing: true, actionError: '', actionResult: null })

    let result
    try {
      result = await this.props.contract.query(actionName, parameters, {
        from: this.state.signer
      })
    } catch (e) {
      console.warn(e)
      this.setState({ executing: false, actionError: e.reason || e.message, actionResult: null })
      return
    }

    this.setState({ executing: false, actionError: '', actionResult: result })
  }

  renderResultContent = () => <ResultContent {...this.state} />
}

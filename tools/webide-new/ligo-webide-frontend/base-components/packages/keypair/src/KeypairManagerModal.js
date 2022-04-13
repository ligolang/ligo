import React, { PureComponent } from 'react'

import {
  Modal,
  ButtonOptions,
  Table,
  Badge,
  IconButton,
  DeleteButton,
  UncontrolledTooltip,
} from '@obsidians/ui-components'

import notification from '@obsidians/notification'

import keypairManager from './keypairManager'

import RevealSecretModal from './RevealSecretModal'
import CreateKeypairModal from './CreateKeypairModal'
import ImportKeypairModal from './ImportKeypairModal'
import KeypairNameModal from './KeypairNameModal'

export default class KeypairManagerModal extends PureComponent {
  static defaultProps = {
    title: 'Keypair Manager',
    warning: true,
    head: ['Name', 'Address', 'Balance'],
    actions: true,
    textActions: ['Create', 'Import'],
    keypairText: 'Keypair',
    RevealSecretModal,
    CreateKeypairModal,
    ImportKeypairModal,
  }

  constructor (props) {
    super(props)

    this.modal = React.createRef()
    this.createKeypairModal = React.createRef()
    this.importKeypairModal = React.createRef()
    this.revealSecretModal = React.createRef()
    this.keypairNameModal = React.createRef()

    this.state = {
      chain: '',
      loading: false,
      keypairs: [],
      keypairFilter: null,
      showPrivateKeys: false,
    }
  }

  openModal = chain => {
    this.modal.current.openModal()
    if (chain) {
      this.setChain(chain)
    }
    this.refresh()
  }

  componentDidMount () {
    this.listenKeypairChange = keypairManager.onUpdated(keypairs => {
      this.setState({ keypairs })
    })
  }

  componentWillUnmount(){
    this.listenKeypairChange && this.listenKeypairChange()
  }

  async refresh () {
    this.setState({ loading: true })
    const keypairs = await keypairManager.loadAllKeypairs()
    this.setState({ keypairs, loading: false })
  }

  setChain = chain => {
    this.setState({ chain })
    if (!chain) {
      this.setState({ keypairFilter: null })
    } else {
      const filter = this.props.chains.find(c => c.key === chain)?.filter
      if (filter) {
        this.setState({ keypairFilter: filter })
      }
    }
  }

  createKeypair = async () => {
    const { keypairText } = this.props
    const success = await this.createKeypairModal.current.openModal(this.state.chain)
    if (success) {
      notification.success(
        `Create ${keypairText} Successful`,
        `A new ${keypairText.toLowerCase()} is created and saved in ${process.env.PROJECT_NAME}.`
      )
      await this.refresh()
    }
  }

  importKeypair = async () => {
    const { keypairText } = this.props
    const success = await this.importKeypairModal.current.openModal(this.state.chain)
    if (success) {
      notification.success(
        `Import ${keypairText} Successful`,
        `The ${keypairText.toLowerCase()} is imported to ${process.env.PROJECT_NAME}.`
      )
      await this.refresh()
    }
  }

  deleteKey = async keypair => {
    const { keypairText } = this.props
    await keypairManager.deleteKeypair(keypair)
    notification.info(
      `Delete ${keypairText} Successful`,
      `The ${keypairText.toLowerCase()} is removed from ${process.env.PROJECT_NAME}.`
    )
    this.refresh()
  }

  revealSecret = keypair => {
    this.revealSecretModal.current.openModal(keypair)
  }

  renderChainOptions () {
    const { chains } = this.props
    const { chain } = this.state

    if (!chains) {
      return null
    }
    return (
      <div>
        <ButtonOptions
          size='sm'
          className='mb-2'
          options={[...chains, { key: '', text: 'All' }]}
          selected={chain}
          onSelect={chain => this.setChain(chain)}
        />
      </div>
    )
  }

  renderKeypairTable = () => {
    let { loading, keypairs, keypairFilter, chain } = this.state
    if (loading) {
      return (
        <tr key='keys-loading' >
          <td align='middle' colSpan={3}>
            <i className='fas fa-spin fa-spinner mr-1' />Loading...
          </td>
        </tr>
      )
    }
    if (keypairs && keypairFilter) {
      keypairs = keypairs.filter(k => keypairFilter(k.address))
    }
    if (!keypairs || !keypairs.length) {
      const { keypairText } = this.props
      let chainName
      if (chain) {
        chainName = this.props.chains.find(c => c.key === chain)?.text
      }
      let placeholder = `No ${keypairText.toLowerCase()}s`
      if (chainName) {
        placeholder += ` for ${chainName.toLowerCase()}`
      }
      return <tr key='keys-none' ><td align='middle' colSpan={3}>({placeholder})</td></tr>
    }
    return keypairs.map(this.renderKeypairRow)
  }

  editName = async keypair => {
    await this.keypairNameModal.current.openModal(keypair)
    this.refresh()
  }

  renderKeypairRow = keypair => {
    // 过滤地址中包含的不合法字符（禁止出现在 html attr 中的字符）
    // filter the illegal address
    const validAddress = keypair?.address?.replaceAll(/[^-_a-zA-Z0-9]/g, '-')
    const { networkManager } = require('@obsidians/eth-network')
    return (
      <tr key={`key-${validAddress}`} className='hover-flex'>
        <td>
          <div className='d-flex' id={`tooltip-${validAddress}`}>
            <span className='text-truncate'>
              {keypair.name ? keypair.name : <span className='text-muted'>(None)</span>}
            </span>
              <UncontrolledTooltip
                  target={`tooltip-${validAddress}`}
              >
                <p>{keypair.name}</p>
              </UncontrolledTooltip>
            {
              !this.props.modifyNameDisabled &&
              <IconButton
                color='transparent'
                className='ml-2 text-muted hover-show'
                onClick={() => this.editName(keypair)}
                icon='fas fa-pencil-alt'
              />
            }
          </div>
        </td>
        <td>
          <div className='d-flex align-items-center'>
            <code className='small'>{networkManager?.sdk?.utils?.formatAddress(keypair.address, networkManager.network.chainId) || keypair.address}</code>
            <span className='text-transparent'>.</span>
            <DeleteButton
              color='primary'
              className='ml-1 hover-show'
              icon='far fa-eye'
              textConfirm={`Click again to reveal ${this.props.secretName.toLowerCase()}`}
              onConfirm={() => this.revealSecret(keypair)}
            />
          </div>
        </td>
        <td>
          <Badge pill color='success' className='ml-1'>{keypair.balance} {networkManager?.symbol}</Badge>
        </td>
        <td align='right'>
        {
          !this.props.deletionDisabled &&
          <DeleteButton
            className='hover-show'
            onConfirm={() => this.deleteKey(keypair)}
          />
        }
        </td>
      </tr>
    )
  }

  render () {
    const {
      title,
      warning,
      head,
      chains,
      mnemonic,
      actions,
      textActions,
      RevealSecretModal,
      CreateKeypairModal,
      ImportKeypairModal,
    } = this.props

    let warningComponent = null
    if (warning) {
      warningComponent = (
        <div className='d-flex flex-row align-items-center mb-3'>
          <div className='h4 m-0 mr-3'><i className='fas fa-exclamation-triangle text-warning' /></div>
          <div>
            <div><b>DO NOT</b> use on mainnet! For development purpose only.</div>
            <div className='small text-muted'>
              For convenience in development, the private keys are saved unencrypted.
            </div>
          </div>
        </div>
      )
    }

    return <>
      <Modal
        size='lg'
        ref={this.modal}
        title={title}
        textActions={actions ? textActions : []}
        textCancel='Close'
        onActions={[this.createKeypair, this.importKeypair]}
      >
        {warningComponent}
        {this.renderChainOptions()}
        <Table
          tableSm
          TableHead={(
            <tr>
              <th style={{ width: '25%' }}>{head[0]}</th>
              <th style={{ width: '58%' }}>{head[1]}</th>
              <th style={{ width: '12%' }}>{head[2]}</th>
              <th></th>
            </tr>
          )}
        >
          {this.renderKeypairTable()}
        </Table>
      </Modal>
      <CreateKeypairModal
        ref={this.createKeypairModal}
        kp={keypairManager.kp}
        chains={chains}
        mnemonic={mnemonic}
        secretName={this.props.secretName}
        keypairs={this.state.keypairs}
      />
      <ImportKeypairModal
        ref={this.importKeypairModal}
        kp={keypairManager.kp}
        chains={chains}
        secretName={this.props.secretName}
        keypairs={this.state.keypairs}
      />
      <RevealSecretModal
        ref={this.revealSecretModal}
        kp={keypairManager.kp}
        secretName={this.props.secretName}
      />
      <KeypairNameModal ref={this.keypairNameModal} />
    </>
  }
}
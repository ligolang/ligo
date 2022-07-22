export default class ClipBoardService {
	async writeText(text) {
		// Guard access to navigator.clipboard with try/catch
		// as we have seen DOMExceptions in certain browsers
		// due to security policies.
		try {
			return await navigator.clipboard.writeText(text)
		} catch (error) {
			console.error(error)
		}

		// Fallback to textarea and execCommand solution
		const activeElement = document.activeElement
		const textAreaEle = document.createElement('textarea')
		textAreaEle.setAttribute('aria-hidden', true)
		
		const textArea = document.body.appendChild(textAreaEle)
		textArea.style.height = '1px'
		textArea.style.width = '1px'
		textArea.style.position = 'absolute'

		textArea.value = text
		textArea.focus()
		textArea.select()
		document.execCommand('copy')

		if (activeElement instanceof HTMLElement) {
			activeElement.focus()
		}

		document.body.removeChild(textArea)

		return
	}
}
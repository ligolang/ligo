install-vscode:
	yarn install && \
	rm -f ./*.vsix && \
	yarn package && \
	code --install-extension *.vsix --force

run-vscode: install-vscode
	code

package:
	# Prerequisites: the LIGO debugger adapter executable must be present
	yarn install && \
	rm -f ./*.vsix && \
	yarn package

install-vscode: package
	code --install-extension *.vsix --force

run-vscode: install-vscode
	code

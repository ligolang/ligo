.PHONY: all copy-bin package

all: package

copy-bin:
	cd ligo-debugger; cp -r $$(stack path --local-install-root)/bin ../vscode-plugin

package:
	$(MAKE) -C ligo-debugger build STACK_DEV_OPTIONS='--fast'
	$(MAKE) copy-bin
	cd vscode-plugin && yarn package && cd ..

install-plugin: package
	@# TODO: this will behave improperly in case of multiple versions of *.vsix
	code --install-extension vscode-plugin/*.vsix
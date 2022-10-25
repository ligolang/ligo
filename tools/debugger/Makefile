.PHONY: all copy-bin prepare-vscode package install-plugin

all: package

copy-bin:
	cd ligo-debugger; cp -r $$(stack path --local-install-root)/bin ../vscode-plugin

prepare-for-vscode:
	$(MAKE) -C ligo-debugger build STACK_DEV_OPTIONS='--fast'
	$(MAKE) copy-bin

package: prepare-for-vscode
	$(MAKE) -C vscode-plugin package

install-plugin: prepare-for-vscode
	$(MAKE) -C vscode-plugin install-plugin

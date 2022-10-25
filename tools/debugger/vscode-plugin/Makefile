.PHONY: all package install-plugin

all: build

package:
	# Prerequisites: the Haskell adapter executable must be present
	rm -r build
	mkdir -p build
	yarn package --out=build

install-plugin: package
	@# TODO: this will behave improperly in case of multiple versions of *.vsix
	code --install-extension build/*.vsix

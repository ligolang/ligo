YARN_PKG_CONFIG_BIN_PATH=$1

case "$(uname -s)" in
    CYGWIN*|MINGW32*|MSYS*)
	PATH_V=$(echo "$YARN_PKG_CONFIG_BIN_PATH;$PATH" | sed 's/;;/;/g')
	PATH_V=$(cygpath -u "$PATH_V")
	;;
    *)
	PATH_V=$PATH
	;;
esac

env PATH="$PATH_V" dune build -p class_group_vdf

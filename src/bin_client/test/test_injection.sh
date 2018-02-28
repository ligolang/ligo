#! /usr/bin/env bash

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

start_node 1

sleep 2

# autogenerated from the demo source
protocol_version="Ps1ZDZdgRP4PFDkzmFpiYtE7gJHioavCMxC96i9zJsK6URwSXSJ"

$admin_client inject protocol "$test_dir/demo"
$admin_client list protocols
$client activate protocol $protocol_version with fitness 1 and key dictator
answ=$($client rpc call /blocks/head/protocol with {} 2>/dev/null)

if ! grep "$protocol_version" <<< $answ ; then
  exit 1
fi

echo
echo End of test
echo

show_logs="no"

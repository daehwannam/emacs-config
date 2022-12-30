#!/usr/bin/sh

THIS_PATH=`realpath $0`
DIR_PATH=`dirname $THIS_PATH`

pkill -f xcape
sh "${DIR_PATH}/load-key-config.sh"


# https://stackoverflow.com/a/246128
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

FILE=.tmux.conf

rm -f $DIR/$FILE
cp ~/$FILE $DIR/

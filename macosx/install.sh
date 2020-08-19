#!/bin/sh

TODAY=`/bin/date '+%Y-%m-%d_%H-%M-%S'`
for file in `ls | grep -v $0`;
do
    HOME_FILE=~/.${file}
    file=`pwd`/${file}
    if [ -e ${HOME_FILE} ]; then
        MV_FILE=${HOME_FILE}_orig_${TODAY}
        echo "Already '${HOME_FILE}'."
        echo "mv ${HOME_FILE} ${MV_FILE}"
        mv ${HOME_FILE} ${MV_FILE}
    fi
    echo "ln -s ${HOME_FILE} ${file}\n"
    ln -s ${file} ${HOME_FILE}
done

# .emacsがあるとダメなので消し
HOME_FILE=~/.emacs
if [ -e ${HOME_FILE} ]; then
    MV_FILE=${HOME_FILE}_orig_${TODAY}
    echo "Already '${HOME_FILE}'."
    echo "mv ${HOME_FILE} ${MV_FILE}"
    mv ${HOME_FILE} ${MV_FILE}
fi

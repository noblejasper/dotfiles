#!/bin/sh

GITHUB_BASE="https://github.com/"
REPOS_NAME=`grep url .git/config | head -n 1 | sed 's|[^u]*url = git@github.com:\([^/]*/[^.]*\).git.*|\1|g'`
BRANCH_NAME="`git branch | grep '*' | awk '{ print $2 }'`"
MODE="$1"

if [ "${MODE}" == "help" -o "${MODE}" == "h" -o "${MODE}" == "-h" ];
then
    echo "Usage: ${0} [pr]"
    exit
fi

if [ "${BRANCH_NAME}" == "master" ];
then
    URL="${GITHUB_BASE}${REPOS_NAME}"
else
    if [ "${MODE}" == "pr" ];
    then
        URL="${GITHUB_BASE}${REPOS_NAME}/compare/${BRANCH_NAME}?expand=1"
    else
        MODE="branch"
        URL="${GITHUB_BASE}${REPOS_NAME}/tree/${BRANCH_NAME}"
    fi
fi

echo "Open ${MODE} mode ${URL}."
open "${URL}"

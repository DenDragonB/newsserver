#!/bin/sh
if [ -n "$1" ]
then
TOKEN="$1"
else
echo -n "Enter Author token: "
read TOKEN
fi
curl -g "http://localhost:8080/draft.edit?token=$TOKEN&id=7&header=NewHeader"
echo
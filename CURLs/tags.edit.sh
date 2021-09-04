#!/bin/sh
if [ -n "$1" ]
then
TOKEN="$1"
else
echo -n "Enter ADMIN token: "
read TOKEN
fi
curl "http://localhost:8080/tag.edit?token=$TOKEN&id=1&name=NewComputer"
echo
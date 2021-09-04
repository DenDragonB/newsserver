#!/bin/sh
if [ -n "$1" ]
then
TOKEN="$1"
else
echo -n "Enter any token: "
read TOKEN
fi
curl "http://localhost:8080/tag.get?token=$TOKEN"
echo
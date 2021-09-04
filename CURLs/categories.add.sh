#!/bin/sh
if [ -n "$1" ]
then
TOKEN="$1"
else
echo -n "Enter ADMIN token: "
read TOKEN
fi
curl "http://localhost:8080/category.add?token=$TOKEN&name=Computers"
echo
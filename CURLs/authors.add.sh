#!/bin/sh
if [ -n "$1" ]
then
TOKEN="$1"
else
echo -n "Enter ADMIN token: "
read TOKEN
fi
curl "http://localhost:8080/author.add?token=$TOKEN&user_id=19&about=Thisisbestauthor"
echo
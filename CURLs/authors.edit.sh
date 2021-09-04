#!/bin/sh
if [ -n "$1" ]
then
TOKEN="$1"
else
echo -n "Enter ADMIN token: "
read TOKEN
fi
curl "http://localhost:8080/author.edit?token=$TOKEN&id=1&user_id=1&about=Edit_Author"
echo
#!/bin/sh
if [ -n "$1" ]
then
TOKEN="$1"
else
echo -n "Enter Author token: "
read TOKEN
fi
curl -g "http://localhost:8080/draft.add?token=$TOKEN&header=NewPost&category_id=1&tags_id=[1]&content=NewContent&main_photo=photo.jpg&photos=[photo1.png,photo2.jpg]"
echo
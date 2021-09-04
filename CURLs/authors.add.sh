#!/bin/sh
echo "token=$TOKEN"
curl "http://localhost:8080/author.add?token=$TOKEN&user_id=19&about=Thisisbestauthor"
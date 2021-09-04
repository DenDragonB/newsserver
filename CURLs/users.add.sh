#!/bin/bash
# user=$(curl "http://localhost:8080/user.add?last_name=Kruzenshtern&first_name=Ivan&name=Parohod&avatar=korabl.jpg&pass=asndmnkj")
# export TOKEN=$(jq '.object[0].token' <<< "$user")
# echo "$user"
TOKEN="fc5fbaf9b53b0b450c8515a226faa3e9"
export TOKEN
echo "TOKEN=$TOKEN"
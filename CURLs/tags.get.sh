#!/bin/sh
tag=$(curl "http://localhost:8080/tag.get?token=admin&id=2")
echo "tag = $tag"
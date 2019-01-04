#!/bin/sh

git add .
git commit -m"$1" || true
git push origin master

#!/bin/sh

git add .
git commit -m"Update the book" || true
git push origin master

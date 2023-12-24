#!/bin/zsh

echo "### create a widget"
WIDGET1=$(curl -s localhost:8000/widget \
  -X POST \
  -H "Content-Type: application/json" \
  -d '{ "name": "small widget" }')
WIDGET1_ID=$(echo $WIDGET1 | jq -r ".id")
echo $WIDGET1 | jq

echo "### create another widget"
WIDGET2=$(curl -s localhost:8000/widget \
  -X POST \
  -H "Content-Type: application/json" \
  -d '{ "name": "large widget" }')
WIDGET2_ID=$(echo $WIDGET2 | jq -r ".id")
echo $WIDGET2 | jq

echo "### fetch all the widgets"
curl -s localhost:8000/widget | jq

echo "### update widget $WIDGET1_ID"
curl -s localhost:8000/widget/$WIDGET1_ID \
  -X PATCH \
  -H "Content-Type: application/json" \
  -d '{ "name": "smallest widget" }' \
  | jq

echo "### fetch widget $WIDGET1_ID"
curl -s localhost:8000/widget/$WIDGET1_ID | jq

echo "### delete widget $WIDGET1_ID"
curl -s localhost:8000/widget/$WIDGET1_ID -X DELETE > /dev/null

echo "### fetch all the widgets"
curl -s localhost:8000/widget | jq

echo "### delete widget $WIDGET2_ID"
curl -s localhost:8000/widget/$WIDGET2_ID -X DELETE > /dev/null

echo "### fetch all the widgets"
curl -s localhost:8000/widget | jq

echo "DONE"


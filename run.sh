#!/usr/bin/env bash

echo "# GIS"
cardano-entropy gis \
  --workspace="$workspace" \
  --end-date-time "$end_date_time" \
  --hours 36

echo "# NYSE"
cardano-entropy nyse \
  --workspace="$workspace" \
  --username="$username" \
  --password="$password" \
  --end-date "$end_date" \
  --exit-delay=0 \
  --days 1

echo "# JMA-QUAKE"
cardano-entropy jma-quake \
  --workspace="$workspace" \
  --end-date-time "$end_date_time" \
  --hours 36

echo "# GEOL-UOA"
cardano-entropy geol-uoa \
  --workspace="$workspace" \
  --end-date-time "$end_date_time" \
  --hours 36

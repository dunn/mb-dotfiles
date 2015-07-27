#!/usr/bin/env bash

echo "$@" | tr -d '\n' | pbcopy

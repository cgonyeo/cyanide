#!/usr/bin/env bash

set -e

source build-docker-rootfs.sh

docker build . -t cyanide

rm -rf out out-* docker-rootfs

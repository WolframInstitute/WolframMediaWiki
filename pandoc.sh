#!/usr/bin/env sh
docker run --rm \
       --volume "$(pwd):/data" \
       --user $(id -u):$(id -g) \
       pandoc/core "$@" -f markdown -t mediawiki --lua-filter=lua/postprocess.lua --extract-media=Documents/Mediawiki --resource-path=.:Documents/Markdown -o Documents/Mediawiki/output.mw


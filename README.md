# Wolfram MediaWiki

Utilities for automating the conversion of Wolfram Notebooks into wikitext for MediaWiki servers.

Designed for managing the [Wolfram Institute Wiki](https://wiki.wolframinstitute.org/wiki/Welcome), but applicable to any MediaWiki installation.

## Current Work

Automating the generation of wiki pages for the 256 elementary cellular automata rules.

## Usage

### Current Method

1. Export your Wolfram Notebook to markdown
2. Convert to wikitext using Pandoc. If you don't want to install the pandoc binary, replace the `pandoc` binary in this command with `pandoc.sh` to use Docker.
   ```bash
   pandoc input.md -f markdown -t mediawiki -o output.wiki
   ```

### Dependencies

- Wolfram Mathematica
- [Pandoc](https://github.com/jgm/pandoc) or Docker

## Roadmap

- [ ] Automate conversion to wikitext without external dependencies
- [ ] Interface with MediaWiki API to publish and update pages
- [ ] Manage dependencies of published wikitext (images, attachments, etc.)
- [ ] Add click-to-copy code functionality, or some other automated way to include source code for figures
# Wolfram MediaWiki

Utilities for automating the conversion of Wolfram Notebooks into wikitext for MediaWiki servers.

Used for managing the [Wolfram Institute Wiki](https://wiki.wolframinstitute.org/wiki/Welcome), but is applicable to any MediaWiki installation.

## Paclets 

### MediaWikiAPI
- Interfaces with the MediaWiki API to upload or download content programatically

### MediaWikiTools
- Converts Wolfram notebooks to Wikitext but not back again. That may be added in the future if it's useful. 

### Dependencies

- Wolfram Mathematica

## Roadmap

- [x] Automate conversion to wikitext without external dependencies
- [x] Interface with MediaWiki API to publish and update pages
- [x] Manage dependencies of published wikitext (images, attachments, etc.) 
- [ ] Add click-to-copy code functionality, or some other automated way to include source code for figures
- [ ] Convert Wikitext back to Wolfram notebooks
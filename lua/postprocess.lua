-- Adjusts image widths and formatting for MediaWiki upload.

local default_width = "500"

function Figure(fig)
  local img = fig.content[1].content[1]
  local mediawiki_syntax = string.format('[[File:%s|thumb|%spx]]',
                                          img.src, default_width)
  return pandoc.Plain{pandoc.RawInline('mediawiki', mediawiki_syntax)}
end

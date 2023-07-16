# Noko

Noko is a tool for viewing logs. It is named after Japanese saws or "nokogiri",
Especially timber saws like Kobiki Nokogiri, Temagari Nokogiri, and Anabiki Noko
saws. You can use it to cut down and filter logs.

## Features
* Parses logfmt logs, with other formats planned in the future
* Uses a small template language to allow your preferred rendering
* Works in both interactive (less-like) and non-interactive modes (grep-like)
* Provides color to your logs to make them easy to read
* Can generate unique colors for cases when many agents are involved
* Can sort by any field, for instance by time
* Can merge many files as well as stdin
* Optimized for low memory usage even on large files
* Fast search through large files
* Lazily indexes your logs on startup so that you don't see a slow down unless
  jump to the end of a large set of logs at which point you should only be limited
  by your read speed. Once all logs have been indexed, usage should be seamless

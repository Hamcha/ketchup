# Howdy, this is Ketchup!

Ketchup is a very small HTTP server implementation, currently [300 LoC total](http://ix.io/gkV)!

Ketchup is also made to be as modular and embeddable as possible, if you want a cabal-free Haskell app, just take those two/three files you need and put it in your project!

Currently Ketchup comes with:

- Basic httpd functionality (IPV4 only for now)
- URL Routing with parameters (ie. `/user/:name/`)
- Static file handler (ie. `static "."` as route/handler)
- Basic Auth (please use it over a HTTPS reverse proxy)

### Dependencies

The core parts of Ketchup are all depedency free, that includes Ketchup.Httpd and Ketchup.Utils (and to a certain extent, Ketchup.Chunked).
The dependencies required are for the following modules:

- Ketchup.Static (uses **mime-types**)
- Ketchup.Auth (uses **base64-bytestring**)

### A word of warning

Ketchup is not a competitor to Snap / Happstack / Yesod / etc., Being designed to be small, Ketchup doesn't have or care about high performance / templating etc., Ketchup doesn't even have HTTPS!

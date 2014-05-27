# Howdy, this is Ketchup!

Ketchup is a very small HTTP server implementation, the HTTPd itself is one file with less than 100 LoC!

Ketchup is also made to be as modular and embeddable as possible, if you want a cabal-free Haskell app, just take those two/three files you need and put it in your project!

Currently Ketchup comes with:

- Basic httpd functionality (IPV4 only for now)
- Regexp routing with parameters (ie. `/user/:name/(.\*)`)
- Static file handler (ie. `static "."` as route/handler)

Most of the project is dependency-free and only uses stuff included in Prelude, with the exception of Ketchup.Static who uses the mime-types package from Hackage, which is a single separately downloadable file.

### A word of warning

Ketchup is not a competitor to Snap / Happstack / Yesod / etc., Being designed to be small, Ketchup doesn't have or care about high performance / templating etc., Ketchup doesn't even have HTTPS!

# Howdy, this is Ketchup!

Ketchup is a very small HTTP server implementation, the HTTPd itself is one file with less than 200 LoC!

Ketchup is also made to be as modular and embeddable as possible, if you want a cabal-free Haskell app, just take those two/three files you need and put it in your project!

### Current structure

Currently Ketchup is made up of 3 files + 1 example.

`Ketchup/Httpd.hs` is the Httpd<br/>
`Ketchup/Routing.hs` is the Router<br/>
`example.hs` is the Example file running a webserver with a couple routes<br/>
You don't have to use the router if you don't need it, just `rm` it and use the global handler from Httpd!

### A word of warning

Ketchup is not a competitor to Snap / Happstack / Yesod / etc., Being designed to be small, Ketchup doesn't have or care about high performance / templating etc., Ketchup doesn't even have HTTPS!
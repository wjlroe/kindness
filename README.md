# kindness

Ludum Dare 32 Entry - An unconventional weapon.

## Usage

### Building everything

    ./script/build-zip.sh

This compiles the ClojureScript to Javascript, compresses the hell out
of it and then adds all the assets to a ZIP file (ready for upload to
Itch.io). In addition, everything in the zip file is available in the
`build/` directory.

You can serve up the production/release game by moving into the
`build` directory and run a static web server in there, like this:

    python -m SimpleHTTPServer

### Generating the soundtrack

    lein run -m kindness.soundtrack

This drops a `soundtrack.wav` file in the current directory. It needs
some editing though, because there tends to be an artifact at the
beginning and the ending is abrupt (the code is "wrong" and needs
musical bug fixing).

### Iterative development

To work on the code, it's most efficient to run figwheel, which will
recompile files when they are saved and push all the new stuff to the
browser you have open (no need to reload usually):

    lein figwheel

By default, when that's running, it'll put up a static web server with
all your development assets (including source maps - so you can debug
easily) at: [localhost:3449](http://localhost:3449).

## License

Copyright © 2015 William Roe

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

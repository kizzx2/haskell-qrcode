# Basic Encoding

The `encode` function and the `toArray` function are all you need. See `Example.hs`. You can use ImageMagick/GraphicsMagick to enlarge the symbol and convert to other formats:

    $ runhaskell Example.hs

    $ # ImageMagick
    $ convert hello.pgm -bordercolor white -border 4 -scale 300x300 -interpolate integer hello.png

    $ # GraphicsMagick
    $ gm convert hello.pgm -bordercolor white -border 4 -scale 300x300 hello.png

Output:

![Example output](/doc/hello.png)

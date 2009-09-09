tablify - a simple utility for formatting CSV files.
 by Daniel Lyons <fusion@storytotell.org>

What it does:

 Tablify is a simple program which does nothing but parse CSV files and render
 them into various display-friendly formats. I spend a fair amount of my time
 dealing with data migration and sometimes it's handy to look at the data
 directly or publish it in such a fashion as other people may find it
 convenient.

 Tablify does not understand every possible CSV format; I took the CSV code
 from the book Real World Haskell. I am not particularly interested in adding
 support for other varieties of CSV because this parser seems to handle the
 common cases and generally if you are getting something else, it's because 
 you asked for something weird. Please let me know if this is not the case for 
 you.

 A major rationale for this application was that I noticed that there are some
 nice characters in the Unicode standard for drawing boxes. I used to draw
 boxes for text files like this:

 +--------------+-----+
 | Name         | Age |
 +--------------+-----+
 | Daniel Lyons | 28  |
 | Reid Givens  | 29  |
 +--------------+-----+

 This works OK (it's what MySQL and PostgreSQL's command line clients do,
 after all.) But how much cooler does this look:

 ┌──────────────┬─────┐
 │ name         │ age │
 ├──────────────┼─────┤
 │ Daniel Lyons │ 28  │
 │ Reid Givens  │ 29  │
 └──────────────┴─────┘

 If you have a nice Unicode font like Menlo on your machine, that should look
 freaking awesome. This is what you get using the -U flag. If you want the old
 fugly ASCII table, use -A.

 Anyway, it occurred to me while writing it that I could quite easily support
 a few other formats, so I wrote an HTML output routine and a TBL output
 routine, TBL being part of troff or groff depending on your system.

 The HTML output routine escapes HTML entities (<, > and &). I expect if you
 actually use TBL you will want to tinker with what it gives you. I haven't
 embellished the system with any justification alternatives besides left
 justification; this is really only an issue for the Unicode output option.

 The program assumes that your input and your output are both UTF-8. There are
 utilities you can use such as iconv to fix files coming in or going out if
 that's not your situation. You may specify many files on the command line, or
 - if you want it to read from standard input.

 Enjoy!

Dependencies:

 • GHC (Glasgow Haskell Compiler) — http://www.haskell.org/ghc/
 • utf8-string library — http://hackage.haskell.org/package/utf8-string
 • bytestring http://hackage.haskell.org/package/bytestring

Building:

 Type `make' at the command line. If everything goes according to plan, 
 you should have a `tablify' binary now.

Installing:

 Move `tablify' to somewhere you can run it from, or add this directory to
 your path.

Contacting me:

 If you run into any bugs or think of a feature you'd like me to add, please
 email me at fusion@storytotell.org and tell me about it.

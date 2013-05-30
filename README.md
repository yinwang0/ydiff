### STATUS OF PARSERS

Please keep in mind that this is an experimental project although some
techniques used here may have interesting innovations. Parsers are not
the focus of this project. The C++ and JavaScript parsers in this
project is experimental and not made for serious uses. More work is
needed to make those parsers able to handle complete languages, but
this is not my currently priority.



### INSTALLATION

ydiff is implemented in Racket. You can get Racket from

    http://racket-lang.org

To build ydiff, go to the directory containing the source and run

    make

It should compile to several executables, such as `diff-lisp`,
`diff-cpp`, `diff-js` etc. Copy those files to some directory in your
PATH.



### USAGE

1. Run commands like this example:

        diff-lisp demos/mk1.ss demos/mk2.ss

   It will produce a HTML file named "mk1-mk2.html" in the current
   directory.


2. Use your browser to open the HTML file. That is basically it.



### Caveats


1. The HTML file needs the support files `nav.js` and `diff.css` to be
   present in the same directory. You must copy those files to the
   directory where you ran the commands. This is not a good user
   experience design and may be improved later.



### Contact

Yin Wang (yinwang0@gmail.com)



### LICENSE

Copyright (C) 2011-2013 Yin Wang

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

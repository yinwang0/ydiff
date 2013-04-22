### INSTALLATION

ydiff is implemented in Racket. You can get Racket from

    http://racket-lang.org

To build ydiff, go to the directory containing the source and run

    make

It should compile to several executables, such as `diff-scheme`,
`diff-cpp`, `diff-js` etc. Copy those files to some directory in your
PATH.



### USAGE

1. Run commands like this example:

        diff-scheme demos/mk1.ss demos/mk2.ss

   It will produce a HTML file named "mk1-mk2.html" in the current
   directory.


2. Use your browser to open the HTML file. That is basically it.



### Caveats


1. The HTML file needs the support files `nav.js` and `diff.css` to be
   present in the same directory. You must copy those files to the
   directory where you ran the commands. This is not a good user
   experience design and may be improved later.


2. Chrome and Opera will not execute JavaScript on local files, so you
   may need to install a web server on your machine to view the file
   correctly. I recommend
   [mongoose](http://code.google.com/p/mongoose). Just put the
   executable in your directory and run it, then visit
   "localhost:8080".

   Other browsers (Safari, Firefox, IE) hasn't this issue and can
   directly open the file.



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

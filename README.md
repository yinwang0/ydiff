### INSTALLATION

ydiff is implemented in Racket. You can get Racket from

    http://racket-lang.org

To build ydiff, go to the directory containing the source and run
"make". It should compile to several executables, such as
"diff-scheme", "diff-cpp" etc. Copy those files to some directory in
your PATH.



### USAGE

1. Run commands like:

        diff-scheme demos/mk.scm demos/mk-c.scm

   It will produce a HTML file named "mk-mk-c.html" in the current
directory.


2. Copy the support files "diff-s.css" and "nav-div.js" to the current
directory, then use your browser to open the HTML file. You can scroll
the file and click on the names to see the interactive effects.

   NOTICE: If you use Chrome, the JavaScript will not run on local files,
so you may need to install a web server on your machine to view the
file correctly. I recommend mongoose:

        http://code.google.com/p/mongoose

   Just put the executable in your directory and run it, then visit
"localhost:8080". 


Have fun!


### Contact

Yin Wang (yinwang0@gmail.com)



### LICENSE

Copyright (C) 2011 Yin Wang

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

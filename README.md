ydiff
=======

*a structural comparison tool for Lisp programs*


### Demo

<a href="http://www.yinwang.org/resources/mk1-mk2.html"><img src="http://www.yinwang.org/images/ydiff3.gif"></a>

### Features

* _Language-aware_. ydiff parses programs, understands basic language constructs and will not make non-sensical comparisons. For example it will not compare a string "10000" with an integer 10000 even though they look very similar. Also, it tries to match functions with the same name before it attempts to destruct and compare functions of different names.

* _Format insensitive_. The comparison result will not be affected by different number of white spaces, line breaks or indentation. For example, ydiff will not produce a large diff just because you surrounded a block of code with if (condition) {...}.

* _Moved code detection_. ydiff can find refactored code -- renamed, moved, reordered, wrapped, lifted, combined or fragmented code. Refactored code can be detected however deep they are into the structures.

* _Human-friendly output_. The output of ydiff is designed for human understanding. The interactive UI helps the user navigate and understand changes efficiently.


### No longer supported Languages

ydiff originally aim to support multiple languages, but I found that the parsers
are very boring and tedious to get right. I hate languages with complicated
syntax which makes parsing hard, so I decided to stop developing parsers for
languages except the Lisp family. The C++ parser here is just to demonstrate how
a mostly-right C++ can be constructed within two days of work ;-) You are
welcome to take the code and extend to complete parsing C++ and JavaScript, but
I can't provide any assistance.


### Installation

ydiff is implemented in Racket. You can get Racket from

    http://racket-lang.org

To build ydiff, go to the directory containing the source and run

    make

It should compile to several executables, such as `diff-lisp`,
`diff-cpp`, `diff-js` etc. Copy those files to some directory in your
PATH.



### Usage

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



#### License (GPLv3)

ydiff - a structural comparison tool for Lisp programs

Copyright (c) 2011-2014 Yin Wang

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

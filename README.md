ydiff
=======

*a structural comparison tool for programs*


### Demo

<a href="http://www.yinwang.org/resources/mk1-mk2.html"><img src="http://yinwang0.files.wordpress.com/2012/01/ydiff3.png?w=600"></a>

### Features

* _Language-aware_. ydiff parses programs, understands basic language constructs and will not make non-sensical comparisons. For example it will not compare a string "10000" with an integer 10000 even though they look very similar. Also, it tries to match functions with the same name before it attempts to destruct and compare functions of different names.

* _Format insensitive_. The comparison result will not be affected by different number of white spaces, line breaks or indentation. For example, ydiff will not produce a large diff just because you surrounded a block of code with if (condition) {...}.

* _Moved code detection_. ydiff can find refactored code -- renamed, moved, reordered, wrapped, lifted, combined or fragmented code. Refactored code can be detected however deep they are into the structures.

* _Human-friendly output_. The output of ydiff is designed for human understanding. The interactive UI helps the user navigate and understand changes efficiently.

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

1. Please keep in mind that this is an experimental project although some
techniques used here may have interesting innovations. Parsers are not
the focus of this project. The C++ and JavaScript parsers in this
project is experimental and not made for serious uses. More work is
needed to make those parsers able to handle complete languages, but
this is not my currently priority.



### Contact

Yin Wang (yinwang0@gmail.com)



### License

Copyright (C) 2011-2013 Yin Wang

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

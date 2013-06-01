;; ydiff - a language-aware tool for comparing programs
;; Copyright (C) 2011-2013 Yin Wang (yinwang0@gmail.com)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



#lang racket

(require "structs.rkt")
(require "utils.rkt")
(require "parse-js.rkt")
(require "diff.rkt")
(require "htmlize.rkt")



;-------------------------------------------------------------
;                         overrides
;-------------------------------------------------------------

(set-get-name
  (lambda (node)
    (let ([name (match-tags node '(name))])
      (cond
       [(not name) #f]
       [(pair? (Node-elts name))
        (get-symbol (car (Node-elts name)))]
       [else #f]))))

;; (same-def? (car (parse1 $statement "function f(x) {}"))
;;            (car (parse1 $statement "function f(x) {}")))


;; command line interface
(let* ([args (current-command-line-arguments)]
       [file1 (vector-ref args 0)]
       [file2 (vector-ref args 1)]
       [text1 (read-file file1)]
       [text2 (read-file file2)]
       [node1 (parse-js text1)]
       [node2 (parse-js text2)]
       [changes (diff node1 node2)])
  (htmlize changes file1 file2 text1 text2))

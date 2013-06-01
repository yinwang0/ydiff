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
(require "parsec.rkt")


(provide parse-js)



;-------------------------------------------------------------
;                     scanner settings
;-------------------------------------------------------------

(define set-parameters
  (lambda ()
    (set-delims (list "("  ")"  "["  "]"  "{"  "}" ","  "`"  ";"))

    (set-operators
     (list
      ">>>="

      "<<=" ">>=" ">>>" "===" "!=="

      "==" "!="  ">=" "<=" "&&" "||" ">>" "<<" "++" "--"
      "+=" "-=" "*=" "/=" "%=" "&=" "^=" "|="

      "=" "+" "-" "*" "/" "%" "~" "!" ":" "?" "." ">" "<"
      ))


    (set-line-comment (list "//"))
    (set-comment-start  "/*")
    (set-comment-end    "*/")
    (set-quotation-marks  '(#\" #\'))
    (set-significant-whitespaces
     (list #\newline #\linefeed #\u2028 #\u2029))))


(set-alpha
  (predor char-alphabetic?
          (lambda (x) (char=? x #\$))))




;-------------------------------------------------------------
;                     primitive parsers
;-------------------------------------------------------------

(:: $identifier
  ($pred
   (lambda (t)
     (and (token? t)
          (id? (Node-elts t))))))


(:: $numeral-literal
  ($pred
   (lambda (t)
     (and (token? t)
          (numeral? (Node-elts t))))))


(:: $string-literal ($pred str?))
(:: $newline ($pred newline?))
(:: $comment ($pred comment?))


;; delimeters
(::  |,|   (@_ ","))
(::  |;|   (@~ ";"))
(::  |:|   (@_ ":"))
(::  |(|   (@~ "("))
(::  |)|   (@~ ")"))
(::  |[|   (@~ "["))
(::  |]|   (@~ "]"))
(::  |{|   (@~ "{"))
(::  |}|   (@~ "}"))


;; $glob^ and $*^ needed to define |\n|, because the
;; definition of |\n| must not contain any call to @seq
;; otherwise the parser will go into infinite loop
(::  |\n|  ($glob^ (@*^ $newline)))
(::  |;\n| (@or |;| |\n|))


;; Redefine sequence to get over newlines
(define old-seq @seq)
(set-seq
  (lambda ps
    (let ([psj (join ps |\n|)])
      (apply old-seq `(,|\n| ,@psj ,|\n|)))))


;; ($eval (@seq ($$ "foo") ($$ "bar"))
;; (scan "
;; foo
;;  bar "))




;-------------------------------------------------------------
;                       compound parsers
;-------------------------------------------------------------

(::= $program 'program
     (@* $statement))



(:: $statement
    (@or $statement-block
         $empty-statement
         $function-definition
         $variable-statement
         $with-statement

         $if-statement
         $switch-statement
         $do-while-statement
         $while-statement
         $for-statement
         $for-in-statement
         $continue-statement
         $break-statement
         $try-statement
         $throw-statement
         $return-statement

         $labelled-statement
         $expression-statement
))



(::= $statement-block 'block
     |{| (@* $statement) |}|
)


(:: $empty-statement |;|)


(::= $function-definition 'function
     ($$ "function") (@= 'name $identifier) $formal-parameter-list
     $function-body)


;; function-expression can be unnamed
(::= $function-expression 'function
     ($$ "function") (@= 'name (@? $identifier)) $formal-parameter-list
     $function-body)


(::= $formal-parameter-list 'parameters
     (@or (@... |(|  (@? (@.@ $identifier |,|))  |)|    )
          $identifier))


(::= $function-body 'body
     $statement-block
)



;;---------------- variable statement -----------------
(::= $variable-statement 'variable-declaration
     ($$ "var") (@.@ $variable-declaration |,|) |;\n|
)


(::= $variable-declaration 'variable-declaration
     $identifier (@? $initializer))


(::= $initializer 'initializer
     (@... ($$ "=") $assignment-expression))


;;--------------------------------------------
(::= $with-statement 'with
     ($$ "with") (@= 'obj |(|  $expression  |)|)
     $statement
)


;;--------------------------------------------
(::= $if-statement 'if
     ($$ "if")  (@= 'test |(| $expression |)|)   $statement
     (@? (@= 'else ($$ "else") $statement
)))


;;--------------------------------------------
(::= $do-while-statement 'do-while
     ($$ "do") $statement
     (@= 'while-do ($$ "while")  (@= 'test |(| $expression |)|   ))
     |;\n|
)


;;--------------------------------------------
(::= $while-statement 'while
     ($$ "while")  (@= 'test |(| $expression |)|   )
     $statement
)


;;--------------------------------------------
(::= $for-statement 'for
     ($$ "for") (@= 'iter
                    |(| (@? $for-initaliser) |;|
                        (@? $expression)     |;|
                        (@? $expression)
                    |)|
                 )
     $statement
)


(::= $for-initaliser 'for-initializer
     (@or (@= 'variable-declaration
              ($$ "var") (@.@ $variable-declaration |,|))

          $expression
))


;;--------------------------------------------
(::= $for-in-statement 'for-in
     ($$ "for") (@= 'iter
                    |(|  (@? $for-in-initalizer) ($$ "in") $expression  |)|)
     $statement
)


(::= $for-in-initalizer 'for-in-initializer
     (@or (@= 'variable-declaration
              ($$ "var") (@.@ $variable-declaration |,|))

          $expression
))


;;--------------------------------------------
(::= $continue-statement 'continue
    ($$ "continue") (@= 'label (@? $identifier)) |;\n|
)


;;--------------------------------------------
(::= $break-statement 'break
     ($$ "break") (@= 'label (@? $identifier)) |;\n|
)


;;--------------------------------------------
(::= $return-statement 'return
    ($$ "return") (@= 'value (@? $expression)) |;\n|
)


;;--------------------------------------------
(::= $labelled-statement 'labelled-statement
     $identifier |:| $statement
)


;;--------------------------------------------
(::= $switch-statement 'switch-statement
     ($$ "switch")  |(| $expression |)|
     |{|  (@* $case-clause)
          (@? $default-clause
          (@* $case-clause))
     |}|
)


(::= $case-clause 'case-clause
     ($$ "case") $expression |:| (@* $statement)
)


(::= $default-clause 'default
     ($$ "default") |:| (@* $statement)
)


;;--------------------------------------------
(::= $throw-statement 'throw
    ($$ "throw") $expression  |;\n|
)


;;--------------------------------------------
(::= $try-statement 'try
     ($$ "try") $statement-block
     (@or $finally-clause
          (@... $catch-clause (@? $finally-clause)))
     )


(::= $catch-clause 'catch
     ($$ "catch") |(| $identifier |)| $statement-block
     )


(::= $finally-clause 'finally
    ($$ "finally") $statement-block
    )


;;--------------------------------------------
(::= $expression-statement 'expression-statement
     $expression |;\n|
     )




;-------------------------------------------------------------
;                       expressions
;-------------------------------------------------------------

;; utility for constructing operators
(define op
  (lambda (s)
    (@= 'op ($$ s))))



(:: $expression
    $comma-expression)



;; 18. comma
;;--------------------------------------------
(::= $comma-expression 'comma
     (@.@ $assignment-expression |,|))



;; 16. assignment
;;--------------------------------------------
(:: $assignment-expression
    (@or (@= 'assignment
             $conditional-expression
             $assignment-operator
             $assignment-expression)

         $conditional-expression
         ))


(:: $assignment-operator
     (@or (op "=")
          (op "*=")
          (op "/=")
          (op "%=")
          (op "+=")
          (op "-=")
          (op "<<=")
          (op ">>=")
          (op ">>>=")
          (op "&=")
          (op "^=")
          (op "|=")
          ))



;; 15.	?:	 Ternary conditional
;;--------------------------------------------
(:: $conditional-expression
    (@or (@= 'conditional-expression
             (@= 'test $logical-or-expression)
             (@~ "?") (@= 'then $conditional-expression)
             (@~ ":") (@= 'else $conditional-expression))

         $logical-or-expression
         ))


; ($eval $conditional-expression (scan "x > 0? x-1 : x"))




;; 14.	||	 Logical OR
;;--------------------------------------------
(:: $logical-or-expression
     (@or (@infix-left 'binop
                       $logical-and-expression
                       (op "||"))

          $logical-and-expression
          ))

; ($eval $logical-or-expression (scan "x || y"))



;; 13.	&&	 Logical AND
;;--------------------------------------------
(:: $logical-and-expression
     (@or (@infix-left 'binop
                       $bitwise-or-expression
                       (op "&&"))

          $bitwise-or-expression
          ))


;; 12.	|	 Bitwise OR (inclusive or)
;;--------------------------------------------
(:: $bitwise-or-expression
     (@or (@infix-left 'binop
                       $bitwise-xor-expression
                       (op "|"))

          $bitwise-xor-expression
          ))



;; 11.	^	 Bitwise XOR (exclusive or)
;;--------------------------------------------
(:: $bitwise-xor-expression
     (@or (@infix-left 'binop
                       $bitwise-and-expression
                       (op "^"))

          $bitwise-and-expression
          ))



;; 10.	&	 Bitwise AND
;;--------------------------------------------
(:: $bitwise-and-expression
     (@or (@infix-left 'binop
                       $equality-expression
                       (op "&"))

       $equality-expression
       ))



;; 9. equality
;;--------------------------------------------
(:: $equality-expression
     (@or (@infix-left 'binop
                       $relational-expression
                       $equality-operator)

          $relational-expression
          ))

(:: $equality-operator
     (@or (op "==")
          (op "!=")
          (op "===")
          (op "!==")
))



;; 8. relational
;;--------------------------------------------
(:: $relational-expression
     (@or (@infix-left 'binop
                       $bitwise-shift-expression
                       $relational-operator)

          $bitwise-shift-expression
          ))

(:: $relational-operator
     (@or (op "<")
          (op "<=")
          (op ">")
          (op ">=")
          (op "instanceof")
          (op "in")
          ))



;; 7. bitwise shift
;;--------------------------------------------
(:: $bitwise-shift-expression
    (@or (@infix-left 'binop
                      $additive-expression
                      $bitwise-shift-operator)

         $additive-expression
))

(:: $bitwise-shift-operator
    (@or (op "<<")
         (op ">>")
         (op ">>>")
         ))



;; 6. additive
;;--------------------------------------------
(:: $additive-expression
    (@or (@infix-left 'binop
                      $multiplicative-expression
                      $additive-operator)

         $multiplicative-expression
))


(:: $additive-operator
    (@or (op "+")
         (op "-")))


;; ($eval $additive-expression (scan "x + y + z"))




;; 5. multiplicative
;;--------------------------------------------
(:: $multiplicative-expression
    (@or (@infix-left 'binop
                      $unary-expression
                      $multiplicative-operator)

         $unary-expression))

(:: $multiplicative-operator
    (@or (op "*")
         (op "/")
         (op "%")))




;; 3. prefix
;; 2. postfix
;;--------------------------------------------
(:: $unary-expression
    $prefix-expression)


(:: $prefix-expression
    (@or (@prefix 'prefix
                  $postfix-expression
                  $prefix-operator)
         $postfix-expression))


(:: $postfix-expression
    (@or (@postfix 'postfix
                   $primary-expression
                   $postfix-operator)
         $primary-expression))


(:: $prefix-operator
    (@or (op "new")
         (op "delete")
         (op "void")
         (op "typeof")
         (op "++")
         (op "--")
         (op "+")
         (op "-")
         (op "~")
         (op "!")
         ))


(:: $postfix-operator
    (@or $index-suffix
         $property-reference-suffix
         $arguments
         (op "++")
         (op "--")))


(::= $index-suffix 'index
     |[|  $expression  |]|
     )


(::= $property-reference-suffix 'property
     (@_ ".") $identifier)


(::= $arguments 'arguments
     |(|  (@? (@.@ $assignment-expression |,|))  |)|
     )


(::= $new-expression 'new
     ($$ "new") $postfix-expression)



;; 1. primary
;;--------------------------------------------
(:: $primary-expression
    (@or $function-expression
         $identifier
         $literal
         (@= 'expression |(|  $expression  |)|  )
         ))




;;-----------
(::= $array-literal 'array-literal
     |[|  (@? (@.@ $assignment-expression |,|))  |]|
     )



;;-----------
(::= $object-literal 'object-literal
     |{|  $property-name-value (@* |,| $property-name-value)  |}|
     )


(::= $property-name-value 'property-name-value
     $property-name |:| $assignment-expression)


(:: $property-name
    (@or $identifier
         $string-literal
         $numeral-literal))



;;-----------
(:: $literal
    (@or ($$ "null")
         ($$ "true")
         ($$ "false")
         (@= 'this ($$ "this"))
         $string-literal
         $numeral-literal
         $array-literal
         $object-literal
))





;-------------------------------------------------------------
;                       parse-js
;-------------------------------------------------------------

(define parse-js
  (lambda (s)
    (set-parameters)
    (first-val
     ($eval $program
            (filter (negate comment?) (scan s))))))




;-------------------------------------------------------------
;                          tests
;-------------------------------------------------------------

;; (parse-js (read-file "demos/nav1.js"))

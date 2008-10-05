; test: vigorsh.trie
; Copyright (C) 2008 kana <http://whileimautomaton.net/>
; License: MIT license  {{{
;     Permission is hereby granted, free of charge, to any person obtaining
;     a copy of this software and associated documentation files (the
;     "Software"), to deal in the Software without restriction, including
;     without limitation the rights to use, copy, modify, merge, publish,
;     distribute, sublicense, and/or sell copies of the Software, and to
;     permit persons to whom the Software is furnished to do so, subject to
;     the following conditions:
; 
;     The above copyright notice and this permission notice shall be included
;     in all copies or substantial portions of the Software.
; 
;     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
; }}}

(use gauche.test)

(test-start "vigorsh.trie")
(add-load-path ".")
(use vigorsh.trie)
(test-module 'vigorsh.trie)




(test-section "(make-trie)")
(define trie (make-trie))
(test* "class" <trie> (class-of trie))
(test* "table" <hash-table> (class-of (slot-ref trie 'table)))


(test-section "(trie-get1)")

(trie-put! trie "foo" 'foo)
(define subtrie-f (trie-get1 trie #\f))
(test* "(trie-get1 trie #\\f)" <trie>
       (class-of subtrie-f))
(test* "... #\\o" <trie>
       (class-of (trie-get1 subtrie-f #\o)))
(test* "... #\\o" <trie>
       (class-of (trie-get1 (trie-get1 subtrie-f #\o) #\o)))
(test* "... #\\X" #f
       (trie-get1 (trie-get1 (trie-get1 subtrie-f #\o) #\o) #\X))

(trie-put! trie "bar" 'bar)
(trie-put! trie "baz" 'baz)
(define subtrie-b (trie-get1 trie #\b))
(define subtrie-z (trie-get1 trie #\z))
(test* "(trie-get1 trie #\\b)" <trie>
       (class-of subtrie-b))
(test* "... #\\a" <trie>
       (class-of (trie-get1 subtrie-b #\a)))
(test* "... #\\r" <trie>
       (class-of (trie-get1 (trie-get1 subtrie-b #\a) #\r)))
(test* "... #\\z" <trie>
       (class-of (trie-get1 (trie-get1 subtrie-b #\a) #\z)))
(test* "(trie-get1 trie #\\z)" #f
       subtrie-z)


(test-section "(trie-get)")
(test* "(trie-get trie \"foo\")" 'foo (trie-get trie "foo"))
(test* "(trie-get trie \"bar\")" 'bar (trie-get trie "bar"))
(test* "(trie-get trie \"baz\")" 'baz (trie-get trie "baz"))
(test* "(trie-get trie \"z\")" #f (trie-get trie "z"))
(test* "(trie-get trie \"foox\")" #f (trie-get trie "foox"))

(test-section "(trie-exists?)")
(test* "(trie-exists? trie \"foo\")" #t (trie-exists? trie "foo"))
(test* "(trie-exists? trie \"bar\")" #t (trie-exists? trie "bar"))
(test* "(trie-exists? trie \"baz\")" #t (trie-exists? trie "baz"))
(test* "(trie-exists? trie \"z\")" #f (trie-exists? trie "z"))
(test* "(trie-exists? trie \"foox\")" #f (trie-exists? trie "foox"))


(test-section "(trie-delete-at!)")
(trie-put! trie "ba" 'XXX)
(test* "(trie-get trie \"ba\")" 'XXX (trie-get trie "ba"))
(test* "(trie-get trie \"bar\")" 'bar (trie-get trie "bar"))
(test* "(trie-get trie \"baz\")" 'baz (trie-get trie "baz"))
(trie-delete-at! trie "ba")
(test* "(trie-get trie \"ba\")" #f (trie-get trie "ba"))
(test* "(trie-get trie \"bar\")" 'bar (trie-get trie "bar"))
(test* "(trie-get trie \"baz\")" 'baz (trie-get trie "baz"))
(trie-delete-at! trie "baz")
(test* "(trie-get trie \"ba\")" #f (trie-get trie "ba"))
(test* "(trie-get trie \"bar\")" 'bar (trie-get trie "bar"))
(test* "(trie-get trie \"baz\")" #f (trie-get trie "baz"))

(test-section "(trie-delete-under!)")
(trie-put! trie "foobar" 'foobar)
(trie-put! trie "foobarbaz" 'foobarbaz)
(test* "(trie-get trie \"foo\")" 'foo (trie-get trie "foo"))
(test* "(trie-get trie \"foobar\")" 'foobar (trie-get trie "foobar"))
(test* "(trie-get trie \"foobarbaz\")" 'foobarbaz (trie-get trie "foobarbaz"))
(trie-delete-under! trie "foobar")
(test* "(trie-get trie \"foo\")" 'foo (trie-get trie "foo"))
(test* "(trie-get trie \"foobar\")" 'foobar (trie-get trie "foobar"))
(test* "(trie-get trie \"foobarbaz\")" #f (trie-get trie "foobarbaz"))
(trie-delete-under! trie "foob")
(test* "(trie-get trie \"foo\")" 'foo (trie-get trie "foo"))
(test* "(trie-get trie \"foobar\")" #f (trie-get trie "foobar"))
(test* "(trie-get trie \"foobarbaz\")" #f (trie-get trie "foobarbaz"))

(test-section "(trie-clear!)")
(trie-clear! trie)
(test* "(trie-get trie \"ba\")" #f (trie-get trie "ba"))
(test* "(trie-get trie \"bar\")" #f (trie-get trie "bar"))
(test* "(trie-get trie \"baz\")" #f (trie-get trie "baz"))
(test* "(trie-get trie \"foo\")" #f (trie-get trie "foo"))
(test* "(trie-get trie \"foobar\")" #f (trie-get trie "foobar"))
(test* "(trie-get trie \"foobarbaz\")" #f (trie-get trie "foobarbaz"))




(test-end)

; __END__
; vim: expandtab softtabstop=2 shiftwidth=2
; vim: filetype=gauche foldmethod=marker

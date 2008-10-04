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
(load "vigorsh.trie")
(import vigorsh.trie)
(test-module 'vigorsh.trie)




(test-section "(make-trie)")
(define trie (make-trie))
(test* "class-of" <trie> (class-of trie))




(test-end)

; __END__
; vim: expandtab softtabstop=2 shiftwidth=2
; vim: filetype=gauche foldmethod=marker

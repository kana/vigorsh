; vigorsh.trie - yet another implementation of trie
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

(define-module vigorsh.trie
  (use gauche.collection)
  (export
    <trie>
    make-trie
    trie-clear!
    trie-delete-at!
    trie-delete-under!
    trie-exists?
    trie-get
    trie-get1
    trie-put!
    ))
(select-module vigorsh.trie)




(define %root (list "root"))
(define %unbound #f)
(define (%make-hash-table)
  (make-hash-table 'eqv?))




(define-class <trie> ()
  ((key :init-keyword :key :init-value %unbound)
   (value :init-keyword :value :init-value %unbound)
   (table :init-form (%make-hash-table))))

(define (make-trie)
  (make <trie>
        :key %root
        :value %root))




(define (trie-get1 trie key)
  ; Return the subtrie of {trie} corresponding to {key}.
  (hash-table-get (slot-ref trie 'table) key  #f))

(define (trie-get-subtrie trie keyseq)
  ; Return the subtrie corresponding to {keyseq} in {trie}.
  (define subtrie trie)
  (for-each
    (lambda (key)
      (when subtrie
        (set! subtrie (trie-get1 subtrie key))))
    keyseq)
  subtrie)

(define (trie-get trie keyseq)
  ; Return the value corresponding to {keyseq} in {trie}.
  (let ((subtrie (trie-get-subtrie trie keyseq)))
    (if subtrie
      (slot-ref subtrie 'value)
      #f)))




(define (trie-put! trie keyseq value)
  (define subtrie trie)
  (for-each
    (lambda (key)
      (let ((%subtrie (trie-get1 subtrie key)))
        (unless %subtrie
          (hash-table-put! (slot-ref subtrie 'table) key (make <trie> :key key))
          (set! %subtrie (trie-get1 subtrie key)))
        (set! subtrie %subtrie)))
    keyseq)
  (slot-set! subtrie 'value value))




(define (trie-clear! trie)
  ; Remove all entries in {trie}.
  (slot-set! trie 'table (%make-hash-table)))

(define (trie-delete-at! trie keyseq)
  ; Remove the entry corresponding to {keyseq} in {trie}.
  (trie-put! trie keyseq %unbound))

(define (trie-delete-under! trie keyseq)
  ; Remove all subentries corresponding to {keyseq} in {trie}.
  (let ((subtrie (trie-get-subtrie trie keyseq)))
    (when subtrie
      (slot-set! subtrie 'table (%make-hash-table)))))




(provide "vigorsh.trie")

; __END__
; vim: expandtab softtabstop=2 shiftwidth=2
; vim: filetype=gauche foldmethod=marker

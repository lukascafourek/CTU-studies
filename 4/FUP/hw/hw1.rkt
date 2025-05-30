#lang racket

(require 2htdp/image)

(provide img->mat ascii-art)

(define chars " .,:;ox%#@")

(define (img->mat img)
  (define width (image-width img))
  (define (RGB->grayscale color)
    (+ (* 0.3 (color-red color))
       (* 0.59 (color-green color))
       (* 0.11 (color-blue color))))
  (define intensities (map RGB->grayscale (image->color-list img)))
  (define (split-list n lst)
    (define (iter l k segment)
      (cond
        [(null? l) (list segment)]
        [(zero? k) (cons segment (iter l n '()))]
        [else (iter (cdr l) (- k 1) (append segment (list (car l))))]))
    (iter lst n '()))
  (split-list width intensities))

(define ((ascii-art width height chars) img)
  (define (split-matrix-into-blocks matrix width height)
    (define list_chars (string->list chars))
    (define (compute-ascii intensity)
      (let ((index (floor (/ (* (length list_chars) (- 255 (floor intensity))) 256))))
        (list-ref list_chars (inexact->exact index))))
    (define (average-intensity block)
      (/ (apply + block) (length block)))
    (define block_height (quotient (length matrix) height))
    (define block_width (quotient (length (car matrix)) width))
    (for/list ([i (in-range 0 block_height)])
      (for/list ([j (in-range 0 block_width)])
        (define block_row_start (* i height))
        (define block_row_end (+ (* i height) height))
        (define block_col_start (* j width))
        (define block_col_end (+ (* j width) width))
        (define block
          (for*/list ([row (in-range block_row_start block_row_end)]
                      [col (in-range block_col_start block_col_end)])
            (list-ref (list-ref matrix row) col)))
        (define average (average-intensity block))
        (compute-ascii average))))
  (define matrix (img->mat img))
  (define blocks (split-matrix-into-blocks matrix width height))
  (define chars_to_string (string-join (map list->string blocks) "\n"))
  (if (string=? chars_to_string "")
      ""
      (string-append chars_to_string "\n")))

;;; Prime number utilities for FoxLisp

;; First 100 prime numbers for hashing and other uses
(defvar lisp::primes
  (list 2 3 5 7 11 13 17 19 23 29
        31 37 41 43 47 53 59 61 67 71
        73 79 83 89 97 101 103 107 109 113
        127 131 137 139 149 151 157 163 167 173
        179 181 191 193 197 199 211 223 227 229
        233 239 241 251 257 263 269 271 277 281
        283 293 307 311 313 317 331 337 347 349
        353 359 367 373 379 383 389 397 401 409
        419 421 431 433 439 443 449 457 461 463
        467 479 487 491 499 503 509 521 523 541))

;; Get the nth prime number (1-indexed)
(defun lisp::prime! (n)
  (nth lisp::primes (- n 1)))

;; Check if a number is prime (simple trial division)
(defun lisp::is-prime (n)
  (if (<= n 1)
      nil
      (if (<= n 3)
          t
          (if (eq 0 (% n 2))
              nil
              (let ((i 3)
                    (limit (+ 1 (%js "Math.sqrt(n)|0")))
                    (is-prime t))
                (loop (and is-prime (< i limit))
                  (if (eq 0 (% n i))
                      (set is-prime nil)
                      (set i (+ i 2))))
                is-prime)))))

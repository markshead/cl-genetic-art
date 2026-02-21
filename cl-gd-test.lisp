;; Had to run this in shell: export DYLD_LIBRARY_PATH=/opt/homebrew/lib:$DYLD_LIBRARY_PATH
;; cl-gd quicklist Makefile needed arch changed and updated to point to homebrew local and libs. Also had to run: Make cl-gd-glue.dylib instead of just make.
(ql:quickload :cl-gd)
(ql:quickload :lparallel)
;;(cffi:load-foreign-library "/opt/homebrew/lib/libgd.dylib")

;;(asdf:oos 'asdf:load-op :cl-gd-test)
;;(cl-gd-test:test)

(defpackage :cl-gd-example (:use :cl :cl-gd :lparallel))



(in-package :cl-gd-example)

;;(defvar target-image-name "./mona-lisa.png")
(defvar target-image-name "./girl-with-pearl-earring.png")
(defvar target-image (cl-gd:create-image-from-file target-image-name))
(defvar width (cl-gd:image-width target-image))
(defvar height (cl-gd:image-height target-image))

;; Create 3 arrays to hold exactly the pixel data of the target image
(defvar target-r (make-array (list (1+ width) (1+ height)) :element-type '(unsigned-byte 8)))
(defvar target-g (make-array (list (1+ width) (1+ height)) :element-type '(unsigned-byte 8)))
(defvar target-b (make-array (list (1+ width) (1+ height)) :element-type '(unsigned-byte 8)))

;; Read the target image entirely into our fast Lisp arrays once
(loop for w from 1 to width do
      (loop for h from 1 to height do
            (let ((colors (cl-gd:color-components
                           (cl-gd:get-pixel w h :image target-image)
                           :image target-image)))
              (setf (aref target-r w h) (first colors))
              (setf (aref target-g w h) (second colors))
              (setf (aref target-b w h) (third colors)))))

(defmacro fast-red (c) `(logand (ash ,c -16) 255))
(defmacro fast-green (c) `(logand (ash ,c -8) 255))
(defmacro fast-blue (c) `(logand ,c 255))

(defmacro fast-allocate-color (r g b a)
  `(let ((val (logior (ash ,a 24) (ash ,r 16) (ash ,g 8) ,b)))
     (if (>= val #x80000000)
         (- val #x100000000)
         val)))

(defvar gene-definition (list 255 255 255 ;; color
			  255         ;; alpha
			  width height;; point
			  width height;; point
			  width height));; point


(defun create-gene (gene-definition)
  (loop for x in gene-definition
	collect (random x )))

(defun create-genome (gene-definition num-genes)
  (loop repeat num-genes
      collect (create-gene gene-definition)))

(defvar current-best-genome (create-genome gene-definition 100))
;; can't score at this point
;;(defvar current-genome-score (score-genome current-best-genome target-image))
(defvar current-genome-score (* height width 255 255 255 255))


(defun mutate-number (original max intensity)
  (let* ((change-amount (random (* 2 intensity)))
	 (change (round (- change-amount intensity)))
	 (value (+ original change)))
    (max 0 (min value max))))

(defun get-pixel-colors ( x y image)
 (cl-gd:color-components
  (cl-gd:get-pixel x y :image image)
  :image image ))


#+nil
(defun test-distribution ()
  (loop for i from 1 to  10000
	count (< 50 (mutate-number 50 100 100))))


(defun mutate-gene (gene gene-def rate-chance rate-change)
  (loop for g in gene
        for gd in gene-def
        collect (if (< (random 100) rate-chance)
                    ;; max 1 ensures random isn't given 0 as an argument during low-change decimals
                    (mutate-number g gd (max 1 (round (/ (* gd rate-change) 100))))
                    g)))

(defun mutate-genome (genome gene-def rate-chance rate-change)
  (loop for g in genome
        collect (mutate-gene g gene-def rate-chance rate-change)))

(defun score-pixel-colors (candidate-colors target-colors)
  (loop for cc in candidate-colors 
	for tc in target-colors
	sum (abs (- tc cc))))



(defun score-genome (genome img)
  "score genome using all pixels."
  (let ((*default-image* img))
   (fill-image 0 0 :color (fast-allocate-color 255 255 255 0))
   (setf (alpha-blending-p) t)
   (setf (save-alpha-p) t)
    (loop for gene in genome
          do (draw-polygon (last gene 6) :filled t
                           :color (fast-allocate-color (first gene) (second gene) (third gene) (fourth gene))))
    (let ((tr (the (simple-array (unsigned-byte 8) (* *)) target-r))
          (tg (the (simple-array (unsigned-byte 8) (* *)) target-g))
          (tb (the (simple-array (unsigned-byte 8) (* *)) target-b)))
      (declare (optimize (speed 3) (safety 0)))
      (loop for w fixnum from 1 to width
            sum (loop for h fixnum from 1 to height
                      for pixel fixnum = (cl-gd:get-pixel (1- w) (1- h) :image *default-image*)
                      for r fixnum = (fast-red pixel)
                      for g fixnum = (fast-green pixel)
                      for b fixnum = (fast-blue pixel)
                      sum (the fixnum (+ (abs (- (the fixnum (aref tr w h)) r))
                                         (abs (- (the fixnum (aref tg w h)) g))
                                         (abs (- (the fixnum (aref tb w h)) b)))) fixnum) fixnum))))

(defun write-genome-image (genome)
  (with-image*
      (width height t)
    (fill-image 0 0 :color (fast-allocate-color 255 255 255 0))
    (setf (alpha-blending-p) t)
    (setf (save-alpha-p) t)
    (loop for gene in genome
	  do
	     (draw-polygon
	      (last gene 6)
	      :filled t
	      :color (fast-allocate-color (first gene) (second gene) (third gene) (fourth gene))))
    (write-image-to-file "current-best-candidate.png" :if-exists :supersede)))

(defun best-of-children (children)
  (let ((best-child nil)
        (best-score most-positive-fixnum))
    (loop for (child score) in children
          do (when (< score best-score)
               (setf best-score score)
               (setf best-child child)))
    (values best-child best-score)))

(defun run (&optional (iterations 10000) (initial-chance 15) (initial-change 25) (num-threads 4))
  ;; Initialize 4 working threads
  (setf lparallel:*kernel* (lparallel:make-kernel num-threads))
  (let ((worker-images (loop repeat num-threads collect (create-image width height t)))
        (rate-chance initial-chance)
        (rate-change initial-change)
        (total-improvements 0)
        (recent-improvements 0)
        (last-100-start-iter 1))
    (setf current-genome-score (score-genome current-best-genome (first worker-images)))
    (format t "Initial score: ~A~%" current-genome-score)
    (unwind-protect
        (time
         (loop
           for x from 1 to iterations
           do

                
              ;; Generate `num-threads` children in true parallel!
              ;; pmapcar farms the work out to the open kernel threads instantly.
              (let* ((children-results
                      (lparallel:pmapcar (lambda (img)
                                           (let* ((multiplier (+ 0.1 (random 2.9)))
                                                  (local-chance (max 1 (round (* rate-chance multiplier))))
                                                  (local-change (max 1 (round (* rate-change multiplier))))
                                                  (child (mutate-genome current-best-genome gene-definition local-chance local-change))
                                                  (score (score-genome child img)))
                                             (list child score)))
                                         worker-images)))
                
                ;; Pick the best candidate out of however many the threads returned
                (multiple-value-bind (new-candidate new-candidate-score) (best-of-children children-results)
                  (if (< new-candidate-score current-genome-score)
                      (progn
                        (incf total-improvements)
                        (incf recent-improvements)
                        (setf current-genome-score new-candidate-score
                              current-best-genome  new-candidate)
                        (write-genome-image current-best-genome)
                        (let* ((total-evals (* x num-threads))
                               (overall-efficiency (if (> total-evals 0) (float (/ total-improvements total-evals)) 0.0))
                               (recent-batches (- x last-100-start-iter))
                               (recent-evals (* recent-batches num-threads))
                               (recent-efficiency (if (> recent-evals 0) (float (/ recent-improvements recent-evals)) 0.0)))
                          (format t "~&score: ~a iter: ~a eval: ~a eff-total: ~,4f eff-recent: ~,4f~%" 
                                  current-genome-score x total-evals overall-efficiency recent-efficiency))))
                
                (when (zerop (mod x 100))
                  (format t ".")
                  (force-output)
                  (setf recent-improvements 0)
                  (setf last-100-start-iter x))))))
      ;; Always guarantee the parallel threads close nicely when the program ends/aborts
      (progn
        (lparallel:end-kernel :wait t)
        (mapc #'destroy-image worker-images)))))

;;(run 1000)



(defvar 1million
'((3 12 10 136 34 261 381 77 182 498) (2 0 0 49 0 119 224 73 419 471)
 (66 148 185 43 202 330 37 92 71 473) (126 13 11 158 299 563 36 189 14 368)
 (19 166 21 117 281 551 185 263 382 335) (7 156 196 122 247 516 483 39 263 24)
 (109 152 185 128 219 478 476 444 395 153) (122 30 0 6 437 534 149 573 187 258)
 (108 142 133 82 278 192 44 308 87 483) (95 63 64 6 426 219 189 270 169 464)
 (17 182 102 123 183 265 54 317 265 16) (64 202 46 23 205 354 92 173 68 333)
 (165 36 69 255 286 178 34 0 437 173) (109 14 40 53 131 395 72 453 355 344)
 (169 164 145 0 374 282 287 71 88 276) (105 0 81 13 210 264 93 433 50 320)
 (114 160 41 65 448 409 164 487 130 316) (15 73 97 47 24 307 297 494 110 388)
 (53 200 179 172 65 354 189 501 53 268) (197 32 5 48 26 162 316 421 99 213)
 (138 127 195 69 364 311 82 426 174 145)
 (187 218 168 0 245 592 386 166 363 104) (0 2 15 45 387 477 447 189 0 230)
 (181 129 29 253 141 15 86 431 0 11) (255 241 61 0 451 389 241 536 257 264)
 (158 144 115 0 216 108 36 382 346 71) (154 150 54 2 239 248 4 158 218 317)
 (47 68 0 117 28 375 18 0 200 110) (29 72 2 97 150 239 23 146 23 186)
 (123 106 91 73 206 562 245 546 0 511) (103 45 177 61 218 437 323 517 274 125)
 (75 16 223 32 344 116 280 306 8 345) (38 30 25 123 4 187 175 558 228 404)
 (222 1 145 72 85 364 265 187 0 327) (255 235 181 128 441 345 4 324 346 71)
 (107 20 192 68 237 187 70 309 325 408) (22 61 16 7 348 344 230 185 30 343)
 (3 136 84 17 269 393 86 243 0 105) (174 223 205 177 195 353 333 70 3 322)
 (25 105 80 13 85 264 63 414 174 293) (14 1 202 255 77 443 357 236 168 158)
 (88 51 36 246 297 571 319 29 0 257) (33 104 171 236 493 312 238 540 457 563)
 (119 24 24 2 260 89 349 145 172 148) (0 16 29 13 425 574 455 395 377 124)
 (0 1 99 10 99 334 379 250 228 415) (25 137 35 102 219 439 83 168 62 449)
 (11 85 97 153 367 129 176 223 379 335) (8 159 7 113 78 466 73 534 61 298)
 (83 56 28 199 500 0 44 411 500 592) (214 214 201 178 300 64 161 592 477 437)
 (37 25 0 54 173 206 0 365 81 139) (36 210 34 84 251 499 292 515 102 475)
 (12 10 49 0 378 445 316 288 12 243) (121 136 14 105 229 507 52 228 76 475)
 (86 9 53 194 220 382 183 242 10 123) (72 181 253 128 378 248 112 345 183 132)
 (14 67 2 0 149 356 219 131 204 227) (90 179 104 141 142 185 345 576 0 60)
 (29 9 109 13 156 373 77 187 171 201) (197 189 146 11 7 365 297 67 321 492)
 (44 1 8 63 392 101 99 252 123 513) (255 159 40 101 465 407 368 74 126 322)
 (4 212 36 120 248 191 384 20 163 414) (80 19 111 49 191 547 143 458 75 335)
 (3 31 1 255 8 15 454 147 11 184) (183 207 232 0 183 126 326 66 278 375)
 (8 9 58 149 137 180 33 236 339 369) (129 46 0 175 261 188 319 436 0 313)
 (151 56 3 0 0 138 381 288 206 570) (184 125 59 104 500 0 0 0 106 350)
 (93 170 215 92 58 243 239 505 442 470) (34 72 80 228 291 545 328 42 48 409)
 (87 113 242 7 189 428 56 389 318 467) (23 53 74 243 247 64 270 79 181 582)
 (93 71 55 127 111 533 451 133 186 86) (58 144 16 127 73 301 63 298 64 460)
 (13 140 138 209 52 326 77 386 32 278) (18 131 153 121 388 152 34 0 0 234)
 (144 137 121 137 323 365 0 389 0 329)
 (240 206 181 168 199 118 219 480 413 550)
 (255 222 196 132 180 163 222 189 48 390) (151 239 161 21 5 23 68 338 416 500)
 (13 183 0 251 141 345 109 213 373 128) (80 66 129 119 18 8 419 282 1 360)
 (21 33 36 155 401 592 368 155 254 181) (67 79 96 73 147 481 306 432 369 42)
 (216 194 155 40 192 324 102 444 173 147) (36 28 17 5 1 592 0 17 496 592)
 (127 156 193 250 8 73 500 93 11 406) (134 128 100 175 331 547 151 192 303 367)
 (124 111 85 6 212 592 147 592 222 440)
 (185 169 126 128 242 361 357 329 178 592)
 (248 204 185 21 158 191 165 325 211 359)
 (113 81 58 27 209 361 245 327 173 268) (99 86 58 168 280 204 178 570 440 592)
 (97 117 132 96 389 95 264 103 482 431)
 (232 196 130 68 269 297 333 592 150 592) (23 145 5 126 239 465 0 78 0 592)
 (250 205 183 68 277 404 151 270 184 125))  )

 (defvar 4million-parallel
 '((103 9 57 38 0 103 33 99 470 18) (68 120 89 72 500 73 363 592 67 72)
 (220 91 0 194 404 528 157 544 449 492) (202 231 0 116 335 317 28 80 319 322)
 (138 190 165 0 78 385 500 511 58 215) (55 16 49 114 425 97 261 343 126 252)
 (105 204 219 102 16 0 106 91 196 35) (219 165 9 125 500 190 29 261 10 498)
 (106 223 27 147 311 56 188 169 426 485) (65 89 4 144 318 73 464 285 100 435)
 (241 121 0 195 65 176 0 30 209 172) (11 222 0 54 181 140 396 110 182 543)
 (233 255 17 213 109 592 410 277 458 46)
 (230 53 122 129 362 149 337 254 70 592) (65 199 130 41 425 337 376 543 317 0)
 (128 255 101 204 49 405 32 418 500 304) (19 51 37 54 446 591 313 592 19 127)
 (46 35 19 147 0 343 500 427 138 0) (200 223 0 0 500 171 493 460 335 457)
 (47 205 48 15 71 517 0 51 459 330) (255 186 29 60 287 592 67 174 167 367)
 (149 255 108 0 39 463 330 443 99 220) (61 0 212 67 183 406 74 282 301 387)
 (0 190 27 2 0 154 219 279 500 35) (6 144 11 70 410 434 171 383 44 436)
 (69 209 162 0 256 488 148 310 500 147) (255 208 0 252 99 0 73 13 138 390)
 (0 27 27 39 234 543 500 421 125 170) (220 0 10 125 441 292 497 395 105 379)
 (125 0 185 232 500 204 61 322 56 511) (47 188 75 211 409 445 31 381 298 26)
 (202 166 16 182 432 108 500 433 310 232)
 (209 205 138 152 393 561 129 231 233 334)
 (195 155 250 255 0 371 187 229 500 527) (24 204 0 124 490 120 0 68 217 327)
 (153 51 68 202 25 175 128 4 257 541) (173 74 182 86 66 0 500 352 0 400)
 (41 0 0 27 160 3 140 196 465 592) (173 165 47 21 250 232 133 222 327 294)
 (63 178 112 41 0 0 174 592 105 0) (100 98 130 50 447 309 106 11 132 0)
 (52 54 0 82 48 523 261 131 123 445) (119 198 126 119 278 220 476 224 0 321)
 (178 109 88 195 271 104 447 508 353 465)
 (163 95 160 181 29 587 284 200 248 303)
 (115 15 88 215 331 186 274 348 419 177) (55 184 65 230 35 585 500 40 94 270)
 (255 0 97 108 26 114 59 375 388 509) (0 141 109 58 241 507 355 0 120 70)
 (0 103 132 132 392 214 119 163 401 236) (184 255 136 219 393 0 88 592 93 243)
 (0 88 126 163 426 39 206 246 54 269) (196 169 116 57 430 574 181 248 234 410)
 (69 166 230 217 61 286 133 426 254 334)
 (200 44 255 255 112 504 107 387 97 492) (255 140 79 52 384 108 454 152 59 60)
 (156 144 185 78 280 69 36 592 294 576) (85 81 0 6 50 0 60 287 0 70)
 (110 10 128 164 190 248 401 505 181 409) (63 81 103 77 384 0 216 172 135 239)
 (101 2 201 194 446 588 64 0 112 462) (140 78 116 208 237 592 53 136 419 313)
 (70 24 255 77 500 321 444 288 212 41) (63 159 110 69 0 551 500 165 153 314)
 (84 0 48 58 402 198 0 324 24 0) (9 10 163 68 274 0 500 387 178 264)
 (255 87 0 143 186 301 116 339 382 426) (1 80 60 0 281 140 428 434 302 398)
 (95 141 0 133 162 317 0 8 380 310) (0 45 37 22 35 488 374 281 337 151)
 (128 125 121 60 81 172 307 92 258 466) (246 8 69 178 293 507 351 132 102 461)
 (0 70 255 163 285 290 208 165 1 436) (23 0 77 15 386 258 181 417 0 7)
 (133 166 81 139 430 467 498 310 430 515) (75 255 111 190 178 0 436 142 22 275)
 (76 21 255 94 146 71 339 223 203 177) (0 156 78 156 0 129 212 423 375 31)
 (139 127 91 4 317 362 417 436 46 440) (0 213 0 109 336 592 0 135 251 592)
 (17 13 44 80 202 592 459 390 286 92) (5 70 164 136 39 530 462 124 247 52)
 (51 192 150 21 118 309 441 87 277 557) (209 0 106 238 392 322 93 212 115 275)
 (110 160 0 57 488 318 435 246 333 527) (62 0 86 33 302 440 217 27 108 0)
 (33 131 155 62 151 316 80 107 226 284) (25 2 24 93 322 58 219 123 360 257)
 (252 0 95 98 61 0 277 231 443 36) (255 164 180 236 323 263 296 221 205 487)
 (58 231 180 0 288 392 338 480 348 188) (37 31 13 3 500 0 500 592 0 0)
 (152 115 91 15 489 437 394 489 358 92) (184 217 168 18 217 112 0 440 349 66)
 (54 33 54 74 36 404 364 362 361 33) (40 31 18 0 0 592 0 0 500 592)
 (12 0 255 242 189 122 79 0 500 486) (227 233 212 231 432 592 165 547 350 62)
 (255 206 129 199 337 592 271 196 169 592)
 (239 211 210 183 343 62 155 177 192 368)))

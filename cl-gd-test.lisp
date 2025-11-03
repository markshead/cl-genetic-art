;; Had to run this in shell: export DYLD_LIBRARY_PATH=/opt/homebrew/lib:$DYLD_LIBRARY_PATH
;; cl-gd quicklist Makefile needed arch changed and updated to point to homebrew local and libs. Also had to run: Make cl-gd-glue.dylib instead of just make.
(ql:quickload :cl-gd)
;;(cffi:load-foreign-library "/opt/homebrew/lib/libgd.dylib")

;;(asdf:oos 'asdf:load-op :cl-gd-test)
;;(cl-gd-test:test)

(defpackage :cl-gd-example (:use :cl :cl-gd))



(in-package :cl-gd-example)

;;(defvar target-image-name "./mona-lisa.png")
(defvar target-image-name "./girl-with-pearl-earring.png")
(defvar target-image (cl-gd:create-image-from-file target-image-name))
(defvar width (cl-gd:image-width target-image))
(defvar height (cl-gd:image-height target-image))

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


(defun mutate-gene (gene gene-def)
  (let ((chance% 2)
	(change% 2))
    (loop for g in gene
	  for gd in gene-def
	  collect (if (< (random 100) chance%)
		      (mutate-number g gd (round (/ (* gd change%) 100)))
		      g))))

(defun mutate-genome (genome gene-def)
  (loop for g in genome
	collect (mutate-gene g gene-def)))

(defun score-pixel-colors (candidate-colors target-colors)
  (loop for cc in candidate-colors 
	for tc in target-colors
	sum (abs (- tc cc))))

(defun score-genome (genome target-image)
  "score genome pixel by pixel against target. Lower is better match."
  (with-image*
   (width height t)
   (allocate-color 255 255 255)
   (setf (alpha-blending-p) t)
   (setf (save-alpha-p) t)
    (loop for gene in genome
	  do
	  (draw-polygon
	    (last gene 6)
	    :filled t
	    :color (allocate-color (first gene) (second gene) (third gene) :alpha (fourth gene))))
   (loop
      for w from 1 to  width 
      sum
      (loop for h from 1 to  height
	    sum 
	       (score-pixel-colors
		   (get-pixel-colors w h *default-image*)
		   (get-pixel-colors w h target-image))))) )

(loop for w from 1 to width
      sum (loop for h from 1 to height
		sum h))

(defun write-genome-image (genome)
  (with-image*
      (width height t)
    (allocate-color 255 255 255)
    (setf (alpha-blending-p) t)
    (setf (save-alpha-p) t)
    (loop for gene in genome
	  do
	     (draw-polygon
	      (last gene 6)
	      :filled t
	      :color (allocate-color (first gene) (second gene) (third gene) :alpha (fourth gene))))
    (write-image-to-file "current-best-candidate.png" :if-exists :supersede)))

(defun run ()
  (loop
    for x from 1 to 10000
    do
       (let* ((new-candidate (mutate-genome current-best-genome gene-definition))
	      (new-candidate-score (score-genome new-candidate target-image))) 
	 (if (< new-candidate-score current-genome-score)
	     (progn
	       (setf current-genome-score new-candidate-score
		     current-best-genome  new-candidate)
	       (write-genome-image current-best-genome)
	       (format t "~&score: ~a iteration: ~a~%" current-genome-score x))
	     (when (zerop (mod x 10)) (format t "."))))))

;;(run)



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

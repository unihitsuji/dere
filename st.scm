;#!
;;; -*- coding: utf-8 -*-
;;;
;;; unit: どのユニットかを意味する
;;;     0: 通常ライブのユニット
;;;     A: GRAND のユニットA
;;;     B: GRAND のユニットB
;;;     C: GRAND のユニットC
;;; spec: 特技発動の　(unit 周期 確率 期間)
;;; spec-desc: 例 ("A9中しばらく" "オルタネイト" "楓")
;;;     car   : string->spec で spec に変換できる文字列
;;;             これを string-spec と呼ぶ
;;;     cadr  : 特技名
;;;     caddr : アイドル名
;;; act?: #f 発動してない, #f 以外 発動している
;;; rise: 特技発動が開始する act? は #f 以外
;;; fall: 特技発動が終了する act? は #f
;;; cover: (始点 終点)
;;; covers: ( (始点1 終点1) (始点2 終点2) )
;;;
(define-module dere.st
  (use gauche.sequence)
  (use gauche.logger)
  (use private.bas)
  (use private.seq)
  (export which-unit symbol->number string->spec spec->covers
          act? act-before act-just-before
          steps scale line hbar hbars
          encore? not-encore?))
(select-module dere.st)
;;; 文字列の先頭でユニットを判別する
(define (which-unit str)
  (let ((c (string-ref str 0)))
    (cond
     ((char=? c #\A) 'A)
     ((char=? c #\B) 'B)
     ((char=? c #\C) 'C)
     ((char-numeric? c) 0)
     (else
      (raise
       (condition (&message (message "can't recognize unit.")) (&error)))))))
;;;
(define (symbol->number sym)
  (cond
   ((eq? sym '中) 0.525)
   ((eq? sym '高) 0.600)
   ((eq? sym '一瞬) 3.0)
   ((eq? sym 'わずか) 4.5)
   ((eq? sym '少し) 6.0)
   ((eq? sym 'しばらく) 7.5)
   ((eq? sym 'かなり) 9.0)
   (else 0)))
;;; 部分文字列を返す
;;;     Gauche 0.9.12 にはあるようだが
;;;     Gauche 0.9.9 にはないようなので作った
;;;     故に export しない
(define (opt-substring str :optional (start 0) (end -1))
  (let ((len (string-length str))
        (end2 (if (= end -1) (string-length str) end)))
    (if (and (zero? start) (= len end2))
        str
        (substring str start end2))))
;;; 文字列を spec に変換する
(define (string->spec str)
  (let* ((u (which-unit str))
         (i (if (eq? 0 u) 0 1))
         (j (+ i (train char-numeric? (opt-substring str i)))))
    (list u
          (exact->inexact (string->number (opt-substring str i j)))
          (string->symbol (opt-substring str j (+ 1 j)))
          (string->symbol (opt-substring str (+ 1 j))))))
;;; spec から covers を作る
(define (spec->covers sec spec)
  (let ((period
         (cond
          ((eq? (car spec)  0) (cadr spec))
          ((eq? (car spec) 'A) (* (cadr spec) 3))
          ((eq? (car spec) 'B) (* (cadr spec) 3))
          ((eq? (car spec) 'C) (* (cadr spec) 3))))
        (start
         (cond
          ((eq? (car spec)  0) (cadr spec))
          ((eq? (car spec) 'A) (cadr spec))
          ((eq? (car spec) 'B) (* (cadr spec) 2))
          ((eq? (car spec) 'C) (* (cadr spec) 3))))
        (duration (symbol->number (cadddr spec))))
    (let loop ((head start) (acc ()))
      (cond
       ((>= head (- sec 3)) (reverse acc))
       ((>= (+ head duration) sec)
        (reverse (cons (list head (exact->inexact sec)) acc)))
       (else
        (loop (+ head period)
              (cons (list head (+ head duration)) acc)))))))
;;; spec-desc から covers を作る
(define (desc->covers sec spec-desc)
  (spec->covers sec (string->spec (car spec-desc))))
;;; 指定した秒 sec が covers に含まれるか？
;;; 含まれているなら、その cover を返し
;;; 含まれていないなら #f を返す
(define (act? sec covers)
  (find
   (lambda (cs) (and (<= (car cs) sec) (< sec (cadr cs))))
   covers))
;;; 指定した秒数　(rise-sec) の直前に発動した
;;; 発動を covers の中から探して cover を返す
(define (act-before rise-sec covers)
  (let loop ((cs covers) (acc ()))
    (cond
     ((null? cs) acc)
     ((<= rise-sec (caar cs)) acc)
     (else (loop (cdr cs) (car cs))))))
;;; spec-desc のリスト spec-descs の中から
;;; 指定した秒数 (rise-sec) の直前の発動(cover)とその spec-desc を返す
;;;     ARGUMENTS
;;;         song-sec 曲の長さ (秒数)
;;;         rise-sec アンコール発動開始の秒数
;;;         spec-descs spec-desc のリスト
;;;     RETURNS
;;;         ( cover spec-desc )
;;;         例 (( 9.0 16.5) ("A9中しばらく" "オルタネイト" "楓") )
(define (act-just-before song-sec rise-sec spec-descs)
  (let loop ((descs spec-descs) (acc '((0 0) ())))
    (cond
     ((null? descs) acc)
     (else
      (let ((new (act-before rise-sec (spec->covers song-sec (string->spec (caar descs))))))
        (log-format "st.scm > act-just-before : new ~S" new)
        (if (and (not (null? new)) (< (caar acc) (car new)))
            (loop (cdr descs) (list new (car descs)))
            (loop (cdr descs) acc)))))))
;;;
(define (steps start end step)
  (iota (ceiling (/ (- end start) step)) start step))
;;; 目盛りを文字列で作る
(define (scale sec :optional (step 0.5))
  (let ((w (floor->exact (/ 10 step)))
        (n (floor->exact (/ sec 10))))
    (apply
     string-append
     (map
      (lambda (x)
        (format #f (format #f "~~~AA" w) (* x 10)))
      (iota (+ 1 n) 0)))))
;;; チャート用の線を文字列で作る
(define (line sec :optional (step 0.5))
  (let ((divisions (inexact->exact (/ 5 step))))
    (list->string
     (map-with-index
      (lambda (i t)
        (cond
         ((zero? (remainder i (* divisions 2))) #\|)
         ((zero? (remainder i (* divisions 1))) #\+)
         (else #\-)))
      (steps 0 sec step)))))
;;; 発動のチャートを文字列で作る
(define (hbar sec covers :optional (step 0.5))
  (list->string
   (let loop ((ts (steps 0 sec step)) (rise #f) (acc ()))
     (cond
      ((null? ts) (reverse acc))
      ((act? (car ts) covers)
       (if rise
           (loop (cdr ts) #t (cons #\= acc))
           (loop (cdr ts) #t (cons #\* acc))))
      (else (loop (cdr ts) #f (cons #\space acc)))))))
;;;
(define (encore?     x)
  (and (not (null? (cdr x))) (equal? (cadr x) "アンコール")))
(define (not-encore? x) (not (encore? x)))
;;;
(define (hbars sec spec-descs :optional (step 0.5))
  (format #t "~A~%" (scale sec step))
  (format #t "~A~%" (line  sec step))
  (for-each
   (lambda (spec-desc)
     (if (null? spec-desc)
         (format #t "~A~%" (line sec step))
         (format #t "~A"
                 (uf (encore? spec-desc)
                      (lambda (x) (color-text '赤 x))
                      nop
                      (format #f "~A ~A~%"
                              (hbar sec (desc->covers sec spec-desc) step)
                              spec-desc)))))
   spec-descs)
  ;;; spec-desc が string-spec だけなら行わない
  ;;; spec-desc で特技がアンコールのものの直前の発動を調べる
  (if (and (not (null? spec-descs)) (< 1 (length (car spec-descs))))
      (let ((encores     (nusk filter encore?     spec-descs))
            (not-encores (nusk filter not-encore? spec-descs)))
        (for-each
         (lambda (spec-desc)
           (format #t "~A~%" spec-desc)
           (for-each
            (lambda (c)
              (log-format "st.scm > hbars ~A ~A" spec-desc c)
              (format #t "  ~A " c)
              (format #t "~A~%" (act-just-before sec (car c) not-encores)))
            (desc->covers sec spec-desc)))
         encores))))

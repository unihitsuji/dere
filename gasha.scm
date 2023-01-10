;#!
;;; -*- coding: utf-8 -*-
;;; ガシャの確率に関する表データを生成する (table コマンド)
(use gauche.test)
(use private.bas)

(define (table n p title)
  (format #t "~A~%" title)
  (format #t "回数, 0枚, 1枚のみ, 1枚以上, 2枚以上, 3枚以上~%")
  (let loop ((counter 1))
    (cond
     ((< n counter) #t)
     (else
      (format #t "~3D, ~18,16:f, ~18,16:f, ~18,16:f, ~18,16:f, ~18,16:f~%"
              counter
              (hit p counter 0)
              (if (<= 1 counter) (hit    p counter 1) "")
              (if (<= 1 counter) (chance p counter 1) "")
              (if (<= 2 counter) (chance p counter 2) "")
              (if (<= 3 counter) (chance p counter 3) ""))
      (loop (+ 1 counter))))))

(define (csv . args)
  ;;(format (current-error-port) "CSV: ~S~%" args)
  (cond
   [(or (null? (cddr args)) (string=? "pu" (list-ref args 2)))
    (table 300 0.0040 "デレステ 恒常/期間限定 ピックアップ")]
   [(string=? "ssr" (list-ref args 2))
    (table 300 0.0300 "デレステ 恒常/期間限定 SSR")]
   [(string=? "fes" (list-ref args 2))
    (table 300 0.0075 "デレステ フェス ピックアップ")]
   [(string=? "fes-ssr" (list-ref args 2))
    (table 300 0.0600 "デレステ フェス SSR")]
   [(string=? "enst" (list-ref args 2))
    (format #t "最高レアリティ (デレステ:SSR / あんスタ:星5) のピックアップが 1枚以上~%")
    (format #t "回数, デレステ恒常/期間限定, デレステフェス限定, あんスタ~%")
    (let loop ((counter 1))
      (cond
       ((< 300 counter) #t)
       (else
        (format #t "~3D, ~D, ~D, ~D~%"
                counter
                (if (<= 1 counter) (chance 0.004  counter 1) "")
                (if (<= 1 counter) (chance 0.0075 counter 1) "")
                (if (<= 1 counter) (chance 0.01   counter 1) ""))
        (loop (+ 1 counter)))))]
   [else #f]))

(define (help . args)
  (format (current-error-port) "HELP: ~S~%" args)
  (formats
   (current-error-port)
   #""
   #"USAGE"
   #"  $ gosh ~(car args)"
   #"      下記のコマンド exam と同じ"
   #""
   #"  $ gosh ~(car args) コマンド"
   #"      exam: 単体テストを行う"
   #"      help: 本ヘルプを表示する"
   #"      csv: デレステ 恒常/期間限定 ピックアップ"
   #""
   #"  $ gosh ~(car args) コマンド 引数 ..."
   #""
   #"      csv pu"
   #"        デレステ 恒常/期間限定 ピックアップ"
   #""
   #"      csv ssr"
   #"        デレステ 恒常/期間限定 SSR"
   #""
   #"      csv fes"
   #"        デレステ フェス限定 ピックアップ"
   #""
   #"      csv fes-ssr"
   #"        デレステ フェス限定 SSR"
   #""
   #"      csv enst"
   #"        デレステ 恒常/期間限定 | フェス限定 | あんスタ ピックアップ"
   #""))

(define (exam . args)
  ;;(format (current-error-port) "EXAM: ~S~%" args)
  ;;; ユニットテスト
  ;;; https://practical-scheme.net/gauche/man/gauche-refj/Dan-Ti-tesuto.html
  (test-start "test for private.bas")
  (test-module 'private.bas)
  (test-section "test group 0 combinations-size")
  (test "test 0-01"  1.0       (^[] (chance 0.004 1 0)))
  (test "test 0-01"  0.004     (^[] (chance 0.004 1 1)))
  (test "test 0-01"  0.007984  (^[] (chance 0.004 2 1)))
  (test "test 0-01"  0.000016  (^[] (chance 0.004 2 2))))

(define (main argv) (invoke argv :proc exam))

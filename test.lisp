(in-package #:cl-user)

(defpackage #:arrows/test
  (:use #:cl #:arrows #:hu.dwim.stefil))

(in-package #:arrows/test)

(defsuite* test-arrows)

(deftest test--> ()
  (is (= (-> 3 /) 1/3))
  (is (= (-> 3 (/)) 1/3))
  (is (= (-> 3 (/ 2)) 3/2))
  (is (= (-> 3 (/ 2) /) 2/3)))

(deftest test-->> ()
  (is (= (->> 3 /) 1/3))
  (is (= (->> 3 (/)) 1/3))
  (is (= (->> 3 (/ 2)) 2/3))
  (is (= (->> 3 (/ 2) /) 3/2)))

(deftest test--<> ()
  (is (= (-<> 3 /) 1/3))
  (is (= (-<> 3 (/)) 1/3))
  (is (= (-<> 3 (/ 2)) 3/2))
  (is (= (-<> 3 (/ 2) /) 2/3))
  (is (= (let ((x 3))
           (-<> (incf x)
                (+ <> <>)))
         8)))

(deftest test--<>> ()
  (is (= (-<>> 3 /) 1/3))
  (is (= (-<>> 3 (/)) 1/3))
  (is (= (-<>> 3 (/ 2)) 2/3))
  (is (= (-<>> 3 (/ 2) /) 3/2))
  (is (= (-<>> (list 1 2 3)
               (remove-if #'oddp <> :count 1 :from-end t)
               (reduce #'+)
               /)
         1/3)))

(deftest test-as-> ()
  (is (= (as-> 3 $
               (* 5 $)
               (/ $ 7))
         15/7))
  (is (= (as-> 0 n
               (1+ n)
               (1+ n))
         2)))

(deftest test-some-> ()
  (is (null (some-> 3
                    (+ 5)
                    (member '(2 5 9))
                    first
                    (* 9))))
  (is (= (some-> 3
                 (+ 5)
                 (member '(2 5 8 9))
                 first
                 (* 9))
         72))
  (is (= (some-> 3
                 (+ 5)
                 (member '(2 5 8 9))
                 second
                 (* 9))
         81))
  (is (null (some-> 3
                    (+ 5)
                    (member '(2 5 8 9))
                    third
                    (* 9))))
  (is (null (some-> '(:a 1)
                    (getf :b)
                    1+))))

(deftest test-some->> ()
  (is (= (some->> '((:a . 3) (:b . 5))
                  (assoc :a)
                  cdr
                  1+)
         4))
  (is (null (some->> '((:a . 3) (:b . 5))
                     (assoc :c)
                     cdr
                     1+))))

(deftest test-cond-> ()
  (is (equal (labels ((strcat (&rest things)
                        (with-output-to-string (s)
                          (dolist (thing things)
                            (when thing (princ thing s)))))
                      (say (n)
                        (cond-> nil
                                ((zerop (mod n 3)) (strcat "Fizz"))
                                ((zerop (mod n 5)) (strcat "Buzz"))
                                (t (or (strcat n))))))
               (mapcar #'say '(9 10 11 12 13 14 15)))
             '("Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz"))))

(deftest test-cond->> ()
  (is (equal (flet ((say (n)
                      (cond->> nil
                               ((zerop (mod n 3)) (cons "Fizz"))
                               ((zerop (mod n 5)) (cons "Buzz"))
                               (t (->* (or (list (princ-to-string n))))
                                  reverse
                                  (apply #'concatenate 'string)))))
               (mapcar #'say '(9 10 11 12 13 14 15)))
             '("Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz"))))

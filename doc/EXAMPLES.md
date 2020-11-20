# Arrows - Examples

## Arrow macros

```lisp
ARROWS> (-> 3
          /)

;; (let ((x 3)) (/ x))

1/3
```

```lisp
ARROWS> (-> 3
          (expt 2))

;; (let ((x 3)) (expt x 2))

9
```

```lisp
ARROWS> (->> 3
          (expt 2))

;; (let ((x 3)) (expt 2 x))

8
```

## Diamond arrow macros

```lisp
ARROWS> (-<> 3
          (/ 2))

;; (let ((x 3)) (/ 3 2))

3/2
```

```lisp
ARROWS> (-<> 3
          (/ <> 2))
          
;; (let ((x 3)) (/ 3 2))

3/2
```

```lisp
ARROWS> (-<> 3
          (/ 2 <>))

;; (let ((x 3)) (/ 2 3))

2/3
```

```lisp
ARROWS> (-<>> (list 1 2 3)
          (remove-if #'oddp <> :count 1 :from-end t) ; substitute <>
          (reduce #'+)                               ; insert last
          /)                                         ; list designator

;; (let ((x (list 1 2 3)
;;       (y (remove-if #'oddp x :count 1 :from-end t))
;;       (z (reduce #'+ y)))
;;  (/ z))

1/3
```

```lisp
ARROWS> (let ((x 3))
          (-<> (incf x)  ; (let ((r (incf x)))
            (+ <> <>)))  ;   (+ r r))

;; (let* ((x 3))
;;        (y (incf x))
;;   (+ y y))

8
```

;; Функція merge-lists-spinning-pairs , яка групує відповідні елементи двох списків, почергово змінюючи їх взаємне розташування в групі
(defun merge-lists-spinning-pairs (lst1 lst2 &optional (flip nil))
  (cond
    ((and (null lst1) (null lst2))
     nil)
    ((null lst1)
     (cons (list (car lst2))
           (merge-lists-spinning-pairs
            nil (cdr lst2) (not flip))))
    ((null lst2)
     (cons (list (car lst1))
           (merge-lists-spinning-pairs
            (cdr lst1) nil (not flip))))
    (t
     (cons (if flip
               (list (car lst2) (car lst1))
               (list (car lst1) (car lst2)))
           (merge-lists-spinning-pairs
            (cdr lst1) (cdr lst2) (not flip))))))

;; Функція, що порівнює отриманий результат з очікуваним та показує чи була пройдена перевірка
(defun check-merge-lists-spinning-pairs (name lst1 lst2 expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (merge-lists-spinning-pairs lst1 lst2) expected)
          name))

;; Тестові набори
(defun test-merge-lists-spinning-pairs ()
  (check-merge-lists-spinning-pairs "test 1" '(1 2 3 4 5) '(a b c d) '((1 a) (b 2) (3 c) (d 4) (5)))
  (check-merge-lists-spinning-pairs "test 2" nil nil nil)
  (check-merge-lists-spinning-pairs "test 3" '(1 2 3 4) '(x y) '((1 x) (y 2) (3) (4)))
  (check-merge-lists-spinning-pairs "test 4" '(a b) '(1 2 3 4) '((a 1) (2 b) (3) (4)))
  (check-merge-lists-spinning-pairs "test 5" '(x) '(y) '((x y)))
  (check-merge-lists-spinning-pairs "test 6" '(1 "x" 3) '(a b c) '((1 a) (b "x") (3 c)))
  (check-merge-lists-spinning-pairs "test 7" '(1 2 3) '(a b c) '((1 a) (b 2) (3 c))))

;; Функція що порівнює елемент з елементом в списку
(defun atom-in-set-p (item set)
  (cond
    ((null set) nil)
    ((equal item (car set)) t)
    (t (atom-in-set-p item (cdr set)))))

;; Предикат list-set-intersect-p , який визначає чи перетинаються дві множини, задані списками атомів, чи ні
(defun list-set-intersect-p (set1 set2)
  (cond
    ((or (null set1) (null set2)) nil)
    ((atom-in-set-p (car set1) set2) t)
    (t (list-set-intersect-p (cdr set1) set2))))

;; Функція, що порівнює отриманий результат з очікуваним та показує чи була пройдена перевірка
(defun check-list-set-intersect-p (name set1 set2 expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (list-set-intersect-p set1 set2) expected)
          name))

;; Тестові набори
(defun test-list-set-intersect-p ()
  (check-list-set-intersect-p "test 1" '(1 2 3) '(4 5 6) nil)
  (check-list-set-intersect-p "test 2" '(1 2 3) '(3 4 5) t)
  (check-list-set-intersect-p "test 3" nil nil nil)
  (check-list-set-intersect-p "test 4" '(1 2 3) nil nil)
  (check-list-set-intersect-p "test 5" nil '(1 2 3) nil)
  (check-list-set-intersect-p "test 6" '(a b c) '(a x y) t)
  (check-list-set-intersect-p "test 7" '(1 2 3) '(9 8 3) t)
  (check-list-set-intersect-p "test 8" '(1 2 "x") '(a b c) nil)
  (check-list-set-intersect-p "test 9" '("x" "y") '("z" "x" "w") t))
 
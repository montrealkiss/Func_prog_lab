;; Допоміжна функція для пошуку мінімального елемента в списку
(defun find-min (lst)
  (if (null (cdr lst))
      (car lst)
      (let ((min-tail (find-min (cdr lst))))
        (if (<= (car lst) min-tail)
            (car lst)
            min-tail))))

;; Допоміжна функція для видалення першого входження елемента (конструктивно)
(defun remove-first-occurrence (val lst)
  (cond ((null lst) nil)
        ((= val (car lst)) (cdr lst)) 
        (t (cons (car lst) (remove-first-occurrence val (cdr lst)))))) 

;; Головна функція сортування вибором (Функціональна)
(defun selection-sort-functional (lst)
  (if (null lst)
      nil
      (let ((min-val (find-min lst)))
        (cons min-val 
              (selection-sort-functional (remove-first-occurrence min-val lst))))))

;; Функція, що порівнює отриманий результат з очікуваним та показує чи була пройдена перевірка
(defun check-selection-sort-functional (name input expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (selection-sort-functional input) expected)
          name))

;; Тестові набори
(defun test-selection-sort-functional ()
  (check-selection-sort-functional "test 1" '(3 1 4 1 5 9 2 6) '(1 1 2 3 4 5 6 9))
  (check-selection-sort-functional "test 2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-selection-sort-functional "test 3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-selection-sort-functional "test 4" '() '())
  (check-selection-sort-functional "test 5" '(2 2 2 1 1) '(1 1 2 2 2)))

;; Головна функція сортування вибором (Імперативна)
(defun selection-sort-imperative (lst)
  (let ((result (copy-list lst)) 
        (len (length lst)))
    (dotimes (i (1- len)) 
      (let ((min-idx i))
        (do ((j (1+ i) (1+ j)))
            ((>= j len))
          (when (< (nth j result) (nth min-idx result))
            (setf min-idx j)))
        (rotatef (nth i result) (nth min-idx result))))
    result))

;; Функція, що порівнює отриманий результат з очікуваним та показує чи була пройдена перевірка
(defun check-selection-sort-imperative (name input expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (selection-sort-imperative (copy-list input)) expected)
          name))

;; Тестові набори
(defun test-selection-sort-imperative ()
  (check-selection-sort-imperative "test 1" '(3 1 4 1 5 9 2 6) '(1 1 2 3 4 5 6 9))
  (check-selection-sort-imperative "test 2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-selection-sort-imperative "test 3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-selection-sort-imperative "test 4" '() '())
  (check-selection-sort-imperative "test 5" '(2 2 2 1 1) '(1 1 2 2 2)))

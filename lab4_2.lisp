;; Функція вищого порядку, що створює замикання для дублювання елементів
(defun duplicate-elements-fn (n &key duplicate-p)
  (lambda (x)
    (if (or (null duplicate-p) (funcall duplicate-p x))
        (make-list n :initial-element x)
        (list x))))

;; Функція, що порівнює отриманий результат з очікуваним та показує чи була пройдена перевірка
(defun check-duplicate (name input n duplicate-p expected)
  (let* ((fn (if duplicate-p 
                 (duplicate-elements-fn n :duplicate-p duplicate-p)
                 (duplicate-elements-fn n)))
         (result (mapcan fn input)))
    (format t "~:[FAILED~;passed~]... ~a~%"
            (equal result expected)
            name n)))

;; Тестові набори
(defun test-duplicate ()
  (check-duplicate "test 1" '(1 2 3) 2 nil '(1 1 2 2 3 3))
  (check-duplicate "test 2" '(1 2 3 4 5) 2 #'evenp '(1 2 2 3 4 4 5))
  (check-duplicate "test 4" '(a b) 3 nil '(a a a b b b))
  (check-duplicate "test 5" '(1 2 3) 5 (constantly nil) '(1 2 3)))

(test-duplicate)
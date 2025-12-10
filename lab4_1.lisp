;; Головна функція сортування вибором (Функціональна)
(defun selection-sort-functional (data &key (key #'identity) (test #'<))
  (let* ((decorated-data (mapcar (lambda (x) (cons (funcall key x) x)) data)))
    (let ((sorted-decorated (selection-sort-aux decorated-data test)))
      (mapcar #'cdr sorted-decorated))))

;; Допоміжна рекурсивна функція 
(defun selection-sort-aux (lst test)
  (if (null lst)
      nil
      (let ((min-pair (find-min-pair lst test)))
        ;; Конструюємо список: min + рекурсія для решти без min
        (cons min-pair 
              (selection-sort-aux (remove-first-pair min-pair lst) test)))))

;; Допоміжна функція для пошуку мінімальної ПАРИ
(defun find-min-pair (lst test)
  (if (null (cdr lst))
      (car lst)
      (let ((min-tail (find-min-pair (cdr lst) test))
            (current (car lst)))
        ;; Порівнюємо КЛЮЧІ (car елементів списку пар) використовуючи TEST
        (if (funcall test (car current) (car min-tail))
            current
            min-tail))))

;; Допоміжна функція для видалення першого входження ПАРИ
(defun remove-first-pair (val lst)
  (cond ((null lst) nil)
        ;; Використовуємо EQ, оскільки видаляємо конкретний об'єкт-пару, знайдений раніше
        ((eq val (car lst)) (cdr lst)) 
        (t (cons (car lst) (remove-first-pair val (cdr lst))))))

;; Функція, що порівнює отриманий результат з очікуваним та показує чи була пройдена перевірка
(defun check-sort (name input expected &rest args)
  (let ((functional-result (apply #'selection-sort-functional input args)))
    (format t "~:[FAILED~;passed~]... ~a (Functional)~%"
            (equal functional-result expected)
            name)))

;; Тестові набори
(defun test-sort ()
  (check-sort "test 1" '(3 1 4 1 5 9 2 6) '(1 1 2 3 4 5 6 9))
  (check-sort "test 2" '(1 2 3 4 5) '(1 2 3 4 5))  
  (check-sort "test 3" '(1 2 3 4 5) '(5 4 3 2 1) :test #'>)              
  (check-sort "test 4" '((3 "c") (1 "a") (2 "b")) '((1 "a") (2 "b") (3 "c")) :key #'car)
  (check-sort "test 5" '("apple" "pie" "banana" "a") '("a" "pie" "apple" "banana") :key #'length)
  (check-sort "test 6" '(12 35 49) '(49 35 12) :key (lambda (x) (mod x 10)) :test #'>))

(test-sort)
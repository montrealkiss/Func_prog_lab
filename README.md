 <p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
 <p align="center">
 <b>Звіт з лабораторної роботи 4</b><br/>
 "Функції вищого порядку та замикання"<br/>
 дисципліни "Вступ до функціонального програмування"
 </p>
 <p align="right"><b>Студент</b>: Юдін Дмитро Олексійович КВ-21</p>
 <p align="right"><b>Рік</b>: 2025</p>
 
### Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
 - використати функції вищого порядку для роботи з послідовностями (де/якщо
це доречно, в разі, якщо функції вищого порядку не були використані при
реалізації л.р. №3);
 - додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями. При
цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом. Використання псевдо-функцій не забороняється, але, за можливості, має бути зменшене до необхідного мінімуму.
## Варіант першої частини 1(21):
Алгоритм сортування вибором за незменшенням.
## Лістинг реалізації першої частини завдання
```lisp
(defun selection-sort-functional (data &key (key #'identity) (test #'<))
  (let* ((decorated-data (mapcar (lambda (x) (cons (funcall key x) x)) data)))
    (let ((sorted-decorated (selection-sort-aux decorated-data test)))
      (mapcar #'cdr sorted-decorated))))

(defun selection-sort-aux (lst test)
  (if (null lst)
      nil
      (let ((min-pair (find-min-pair lst test)))
        (cons min-pair 
              (selection-sort-aux (remove-first-pair min-pair lst) test)))))

(defun find-min-pair (lst test)
  (if (null (cdr lst))
      (car lst)
      (let ((min-tail (find-min-pair (cdr lst) test))
            (current (car lst)))
        (if (funcall test (car current) (car min-tail))
            current
            min-tail))))

(defun remove-first-pair (val lst)
  (cond ((null lst) nil)
        ((eq val (car lst)) (cdr lst)) 
        (t (cons (car lst) (remove-first-pair val (cdr lst))))))
```
### Тестові набори та утиліти першої частини завдання
```lisp
(defun check-sort (name input expected &rest args)
  (let ((functional-result (apply #'selection-sort-functional input args)))
    (format t "~:[FAILED~;passed~]... ~a (Functional)~%"
            (equal functional-result expected)
            name)))

(defun test-sort ()
  (check-sort "test 1" '(3 1 4 1 5 9 2 6) '(1 1 2 3 4 5 6 9))
  (check-sort "test 2" '(1 2 3 4 5) '(1 2 3 4 5))  
  (check-sort "test 3" '(1 2 3 4 5) '(5 4 3 2 1) :test #'>)              
  (check-sort "test 4" '((3 "c") (1 "a") (2 "b")) '((1 "a") (2 "b") (3 "c")) :key #'car)
  (check-sort "test 5" '("apple" "pie" "banana" "a") '("a" "pie" "apple" "banana") :key #'length)
  (check-sort "test 6" '(12 35 49) '(49 35 12) :key (lambda (x) (mod x 10)) :test #'>))
### Тестування першої частини завдання
```lisp

```
## Варіант другої частини 9(21):
Написати функцію duplicate-elements-fn, яка має один основний параметр n та один ключовий параметр — функцію duplicate-p. duplicate-elements-fn має повернути функцію, яка при застосуванні в якості першого аргументу mapcan робить наступне: кожен елемент списка-аргумента mapcan, для якого функція duplicate-p повертає значення t (або не nil), дублюється n разів. Якщо користувач не передав функцію duplicate-p у duplicate-elements-fn , тоді дублюються всі елементи вхідного списку.
```lisp
CL-USER> (mapcan (duplicate-elements-fn 2) '(1 2 3))
(1 1 2 2 3 3)
CL-USER> (mapcan (duplicate-elements-fn 2 :duplicate-p #'evenp) '(1 2 3 4 5))
(1 2 2 3 4 4 5)
```
## Лістинг реалізації другої частини завдання 
```lisp
(defun duplicate-elements-fn (n &key duplicate-p)
  (lambda (x)
    (if (or (null duplicate-p) (funcall duplicate-p x))
        (make-list n :initial-element x)
        (list x))))
```
### Тестові набори та утиліти другої частини завдання
```lisp
(defun check-duplicate (name input n duplicate-p expected)
  (let* ((fn (if duplicate-p 
                 (duplicate-elements-fn n :duplicate-p duplicate-p)
                 (duplicate-elements-fn n)))
         (result (mapcan fn input)))
    (format t "~:[FAILED~;passed~]... ~a~%"
            (equal result expected)
            name n)))

(defun test-duplicate ()
  (check-duplicate "test 1" '(1 2 3) 2 nil '(1 1 2 2 3 3))
  (check-duplicate "test 2" '(1 2 3 4 5) 2 #'evenp '(1 2 2 3 4 4 5))
  (check-duplicate "test 4" '(a b) 3 nil '(a a a b b b))
  (check-duplicate "test 5" '(1 2 3) 5 (constantly nil) '(1 2 3)))
```
### Тестування другої частини завдання
```lisp

```

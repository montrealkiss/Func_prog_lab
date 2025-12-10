 <p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
 <p align="center">
 <b>Звіт з лабораторної роботи 3</b><br/>
 "Конструктивний і деструктивний підходи до роботи зі списками"<br/>
 дисципліни "Вступ до функціонального програмування"
 </p>
 <p align="right"><b>Студент</b>: Юдін Дмитро Олексійович КВ-21</p>
 <p align="right"><b>Рік</b>: 2025</p>
 
## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку. Не допускається використання: псевдо-функцій, деструктивних операцій,
циклів . Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і деструктивних функцій (псевдофункцій). Не допускається використання функцій вищого порядку або функцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Тим не менш, оригінальний список цей варіант реалізації також не має змінювати, тому перед виконанням деструктивних змін варто застосувати функцію copy-list (в разі необхідності). Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).
Алгоритм, який необхідно реалізувати, задається варіантом.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести мають бути оформленні у вигляді модульних тестів.
## Варіант 1(21):
Алгоритм сортування вибором за незменшенням.
## Лістинг функції з використанням конструктивного підходу
```lisp
(defun find-min (lst)
  (if (null (cdr lst))
      (car lst)
      (let ((min-tail (find-min (cdr lst))))
        (if (<= (car lst) min-tail)
            (car lst)
            min-tail))))

(defun remove-first-occurrence (val lst)
  (cond ((null lst) nil)
        ((= val (car lst)) (cdr lst)) 
        (t (cons (car lst) (remove-first-occurrence val (cdr lst)))))) 

(defun selection-sort-functional (lst)
  (if (null lst)
      nil
      (let ((min-val (find-min lst)))
        (cons min-val 
              (selection-sort-functional (remove-first-occurrence min-val lst))))))
```
### Тестові набори та утиліти
```lisp
(defun check-selection-sort-functional (name input expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (selection-sort-functional input) expected)
          name))

(defun test-selection-sort-functional ()
  (check-selection-sort-functional "test 1" '(3 1 4 1 5 9 2 6) '(1 1 2 3 4 5 6 9))
  (check-selection-sort-functional "test 2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-selection-sort-functional "test 3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-selection-sort-functional "test 4" '() '())
  (check-selection-sort-functional "test 5" '(2 2 2 1 1) '(1 1 2 2 2)))
```
### Тестування
```lisp
(test-selection-sort-functional)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
NIL
```
## Лістинг функції з використанням деструктивного підходу
```lisp
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
```
### Тестові набори та утиліти
```lisp
(defun check-selection-sort-imperative (name input expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (selection-sort-imperative (copy-list input)) expected)
          name))

(defun test-selection-sort-imperative ()
  (check-selection-sort-imperative "test 1" '(3 1 4 1 5 9 2 6) '(1 1 2 3 4 5 6 9))
  (check-selection-sort-imperative "test 2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-selection-sort-imperative "test 3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-selection-sort-imperative "test 4" '() '())
  (check-selection-sort-imperative "test 5" '(2 2 2 1 1) '(1 1 2 2 2)))
```
### Тестування
```lisp
(test-selection-sort-imperative)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
NIL
```

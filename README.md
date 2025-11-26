 <p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
 <p align="center">
 <b>Звіт з лабораторної роботи 2</b><br/>
 "Рекурсія"<br/>
 дисципліни "Вступ до функціонального програмування"
 </p>
 <p align="right"><b>Студент</b>: Юдін Дмитро Олексійович КВ-21</p>
 <p align="right"><b>Рік</b>: 2025</p>
 
## Загальне завдання
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за
можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно
реалізувати, задаються варіантом. Вимоги до функцій:

1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового
списку, а не зміни наявного (вхідного).

2. Не допускається використання функцій вищого порядку чи стандартних функцій
для роботи зі списками, що не наведені в четвертому розділі навчального
посібника.

3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції
в якості аргументів.

4. Не допускається використання псевдофункцій (деструктивного підходу).

5. Не допускається використання циклів.

Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів.

Додатковий бал за лабораторну роботу можна отримати в разі виконання всіх наступних
умов:
- робота виконана до дедлайну (включно з датою дедлайну);
- крім основних реалізацій функцій за варіантом, також реалізовано додатковий
варіант однієї чи обох функцій, який працюватиме швидше за основну реалізацію,
не порушуючи при цьому перші три вимоги до основної реалізації (вимоги 4 і 5
можуть бути порушені), за виключенням того, що в разі необхідності можна також
використати стандартну функцію copy-list. 
## Варіант 6(21):
1. Написати функцію merge-lists-spinning-pairs , яка групує відповідні елементи
 двох списків, почергово змінюючи їх взаємне розташування в групі:
```
 CL-USER> (merge-lists-spinning-pairs '(1 2 3 4 5) '(a b c d))
 ((1 A) (B 2) (3 C) (D 4) (5))
```
2. Написати предикат  list-set-intersect-p , який визначає чи перетинаються дві
 множини, задані списками атомів, чи ні:
```
 CL-USER> (list-set-intersect-p '(1 2 3) '(4 5 6))
 NIL
 CL-USER> (list-set-intersect-p '(1 2 3) '(3 4 5))
 T
```
## Лістинг функції merge-lists-spinning-pairs:
```lisp
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
```
### Тестові набори
```lisp
(defun check-merge-lists-spinning-pairs (name lst1 lst2 expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (merge-lists-spinning-pairs lst1 lst2) expected)
          name))

(defun test-merge-lists-spinning-pairs ()
  (check-merge-lists-spinning-pairs "test 1" '(1 2 3 4 5) '(a b c d) '((1 a) (b 2) (3 c) (d 4) (5)))
  (check-merge-lists-spinning-pairs "test 2" nil nil nil)
  (check-merge-lists-spinning-pairs "test 3" '(1 2 3 4) '(x y) '((1 x) (y 2) (3) (4)))
  (check-merge-lists-spinning-pairs "test 4" '(a b) '(1 2 3 4) '((a 1) (2 b) (3) (4)))
  (check-merge-lists-spinning-pairs "test 5" '(x) '(y) '((x y)))
  (check-merge-lists-spinning-pairs "test 6" '(1 "x" 3) '(a b c) '((1 a) (b "x") (3 c)))
  (check-merge-lists-spinning-pairs "test 7" '(1 2 3) '(a b c) '((1 a) (b 2) (3 c))))
```
### Тестування
```
(test-merge-lists-spinning-pairs)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
passed... test 6
passed... test 7
NIL
```
## Лістинг функції list-set-intersect-p:
```lisp
(defun atom-in-set-p (item set)
  (cond
    ((null set) nil)
    ((equal item (car set)) t)
    (t (atom-in-set-p item (cdr set)))))

(defun list-set-intersect-p (set1 set2)
  (cond
    ((or (null set1) (null set2)) nil)
    ((atom-in-set-p (car set1) set2) t)
    (t (list-set-intersect-p (cdr set1) set2))))
```
### Тестові набори
```lisp
(defun check-list-set-intersect-p (name set1 set2 expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (list-set-intersect-p set1 set2) expected)
          name))

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
```
### Тестування
```
(test-list-set-intersect-p)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
passed... test 6
passed... test 7
passed... test 8
passed... test 9
NIL
```

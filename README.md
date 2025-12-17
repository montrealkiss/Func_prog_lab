 <p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
 <p align="center">
 <b>Звіт з лабораторної роботи 5</b><br/>
 "Робота з базою даних"<br/>
 дисципліни "Вступ до функціонального програмування"
 </p>
 <p align="right"><b>Студент</b>: Юдін Дмитро Олексійович КВ-21</p>
 <p align="right"><b>Рік</b>: 2025</p>
 
## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури та/або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів. Значення колонок мають бути розібрані відповідно до типу даних у них. Наприклад, рядок — це просто рядок; числові колонки необхідно розібрати як цілі числа або числа з рухомою крапкою.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у select . При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від варіанту):
- структури у геш-таблиці
- геш-таблиці у асоціативні списки
- асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці (pretty-print).

## Варіант 9(21):
База даних: наукові статті.
Тип записів: асоціативний список.

## Лістинг реалізації завдання
```lisp
(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun parse-value (str)
  (let ((val (ignore-errors (parse-integer str))))
    (or val str)))

(defun row-to-alist (headers row)
  (loop for key in headers
        for val in row
        collect (cons key (parse-value (string-trim " " val)))))

(defun read-csv (file-path)
  (with-open-file (stream file-path :direction :input)
    (let* ((header-line (read-line stream nil))
           (headers (when header-line
                      (mapcar (lambda (s) (intern (string-upcase (string-trim " " s)) :keyword))
                              (split-string header-line #\,))))
           (records '()))
      (loop for line = (read-line stream nil)
            while line
            do (let ((row (split-string line #\,)))
                 (push (row-to-alist headers row) records)))
      (nreverse records))))

(defun select (file-path)
  (let ((records (read-csv file-path)))
    (lambda (&rest filters &key &allow-other-keys)
      (if (null filters)
          records
          (remove-if-not
           (lambda (record)
             (loop for (key value) on filters by #'cddr
                   always (equal (cdr (assoc key record)) value)))
           records)))))

(defun alist-to-csv-row (record headers)
  (format nil "~{~A~^,~}"
          (mapcar (lambda (key)
                    (let ((val (cdr (assoc key record))))
                      (if (numberp val) val (format nil "~A" val))))
                  headers)))

(defun save-records (file-path records)
  (when records
    (let ((headers (mapcar #'car (first records))))
      (with-open-file (stream file-path :direction :output :if-exists :supersede)
        (format stream "~{~A~^,~}~%" headers)
        (dolist (rec records)
          (format stream "~A~%" (alist-to-csv-row rec headers)))))))

(defun alist-to-hash-table (alist)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (car pair) ht) (cdr pair)))
    ht))

(defun convert-all-to-hash (records)
  (mapcar #'alist-to-hash-table records))

(defun pretty-print (records)
  (when records
    (let* ((headers (mapcar #'car (first records)))
           (col-widths (mapcar (lambda (h) (length (string h))) headers)))
      (dolist (rec records)
        (loop for key in headers
              for i from 0
              do (setf (nth i col-widths)
                       (max (nth i col-widths)
                            (length (format nil "~A" (cdr (assoc key rec))))))))

      (format t "~&|")
      (loop for h in headers
            for w in col-widths
            do (format t " ~vA |" w h))
      (format t "~%")
      
      (format t "|")
      (loop for w in col-widths
            do (format t "~v@{~A~:*~}|" (+ w 2) "-"))
      (format t "~%")

      (dolist (rec records)
        (format t "|")
        (loop for h in headers
              for w in col-widths
              do (format t " ~vA |" w (cdr (assoc h rec))))
        (format t "~%")))))
```

### Тестові набори та утиліти
```lisp
(defun create-test-files ()
  (with-open-file (s "specialties.csv" :direction :output :if-exists :supersede)
    (format s "ID,NAME,FIELD~%")
    (format s "1,Computer Science,IT~%")
    (format s "2,Physics,Science~%")
    (format s "3,Economics,Social"))
  
  (with-open-file (s "articles.csv" :direction :output :if-exists :supersede)
    (format s "ID,SPECIALTY-ID,TITLE,AUTHOR,YEAR~%")
    (format s "101,1,AI Algorithms,John Doe,2023~%")
    (format s "102,1,Lisp Programming,Jane Smith,2024~%")
    (format s "103,2,Quantum Mechanics,Albert E.,1920~%")
    (format s "104,3,Market Trends,Adam S.,2022~%")))

(defun demo ()
  (create-test-files)
  
  (let ((articles-db (select "articles.csv")))
    
    (format t "~%1. Всі статті:~%")
    (pretty-print (funcall articles-db))
    
    (format t "~%2. Статті за 2024 рік:~%")
    (pretty-print (funcall articles-db :year 2024))
    
    (format t "~%3. Статті зі SPECIALTY-ID = 1 (Computer Science):~%")
    (let ((cs-articles (funcall articles-db :specialty-id 1)))
      (pretty-print cs-articles)

      (format t "~%4. Конвертація першого знайденого запису у Hash Table:~%")
      (let ((ht (alist-to-hash-table (first cs-articles))))
        (maphash (lambda (k v) (format t "Key: ~A, Value: ~A~%" k v)) ht))
      
      (save-records "filtered_articles.csv" cs-articles)
      (format t "~%Результат фільтрації збережено у 'filtered_articles.csv'~%"))))

(demo)
```

### Тестування 
```lisp
1. Всі статті:
| ID  | SPECIALTY-ID | TITLE             | AUTHOR     | YEAR |
|-----|--------------|-------------------|------------|------|
| 101 | 1            | AI Algorithms     | John Doe   | 2023 |
| 102 | 1            | Lisp Programming  | Jane Smith | 2024 |
| 103 | 2            | Quantum Mechanics | Albert E.  | 1920 |
| 104 | 3            | Market Trends     | Adam S.    | 2022 |

2. Статті за 2024 рік:
| ID  | SPECIALTY-ID | TITLE            | AUTHOR     | YEAR |
|-----|--------------|------------------|------------|------|
| 102 | 1            | Lisp Programming | Jane Smith | 2024 |

3. Статті зі SPECIALTY-ID = 1 (Computer Science):
| ID  | SPECIALTY-ID | TITLE            | AUTHOR     | YEAR |
|-----|--------------|------------------|------------|------|
| 101 | 1            | AI Algorithms    | John Doe   | 2023 |
| 102 | 1            | Lisp Programming | Jane Smith | 2024 |

4. Конвертація першого знайденого запису у Hash Table:
Key: ID, Value: 101
Key: SPECIALTY-ID, Value: 1
Key: TITLE, Value: AI Algorithms
Key: AUTHOR, Value: John Doe
Key: YEAR, Value: 2023

Результат фільтрації збережено у 'filtered_articles.csv'
```


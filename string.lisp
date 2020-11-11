(setq myText "Oh my God. I did it. I don't even understand how.")

(defun countWord (str counter i)
(cond
((string-equal (string(char str i)) ".") (+ counter 1))
((string-equal (string(char str i)) " ") (countWord str (+ counter 1) (+ i 1)))
(T (countWord str counter (+ i 1)))
)
)

(defun makeText (text start end)
(cond
((>= end (length text)) NIL)
((string-equal (string(char text end)) ".") (cons (list (subseq text start (+ end 1)) (countWord (subseq text start (+ end 1)) 0 0)) (makeText text (+ end 2) (+ end 2))))
((string-not-equal (string(char text end)) ".") (makeText text start (+ end 1)))
)
)

(defun maxList (text)
(cond
((NULL (CDR text)) (CAR text))
((> (CADAR text) (CADADR text)) (maxList (cons (CAR text) (CDDR text))))
(T (maxList (CDR text)))
)
)

(defun makeWordAndLength (text)
(cond
((NULL text) NIL)
(T (cons (lengthWord (CAAR text) 0 0) (makeWordAndLength (CDR text))))
)
)
(defun lengthWord (list start end)
(cond
((>= end (length list)) NIL)
((or (string-equal (string(char list end)) " ") (string-equal (string(char list end)) ".")) (cons (list (subseq list start end) (- end start)) (lengthWord list (+ end 1) (+ end 1))))
((string-not-equal (string(char list end)) " ") (lengthWord list start (+ end 1)))
)
)

(defun maxWordOfText (text)
(cond
((NULL text) NIL)
(T (cons (maxList (CAR text)) (maxWordOfText (CDR text))))
)
)

(print(makeText myText 0 0))
(print(maxList (makeText myText 0 0)))
(print(makeWordAndLength (makeText myText 0 0)))
(print(maxWordOfText(makeWordAndLength (makeText myText 0 0))))
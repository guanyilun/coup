(import [hylab.core [*]])
(require [hylab.core [*]])

(defparameter *rules*
  ; (action coin char block reaction name)
  '((income +1 (everyone) () () "i")
    (foreign-aid +2 (everyone) (duke) () "f")
    (coup -7 (everyone) () () lose-influence "c")
    (taxes +3 (duke) () () "t")
    (assassinate -3 (assassin) (contessa) lose-influence "a")
    (steal 0 (captain) (capton ambassador) stolen "st")
    (swap 0 (ambassador) () () "sw")))


(defclass Player
  (defun --init-- (self name)
    (setf self.name name)
    (setf self.ncoins 0)
    (setf self.action None)
    (setf self.cards []))
  
  (defun income (self)
    (incf self.ncoins))
  
  (defun foreign-aid (self)
    (incf self.ncoins 2))

  (defun coup (self player)
    (if (>= self.ncoins 7)
        (progn (decf self.ncoins 7)
               (.lose-influence player))))
  
  (defun taxes (self)
    (incf self.ncoins 3))

  (defun assassinate (self player)
    (if (>= self.ncoins 3)
        (progn (decf self.ncoins 3)
               (.lose-influence player))))
  (defun steal (self player)
    (let ((ncoin (min player.ncoins 2)))
      (incf player.ncoins ncoin)
      (decf self.ncoins ncoin)))

  (defun swap (self cards)
    (.draw self)
    (.draw self)
    (list (.discard self) (.discard self)))

  (defun draw (self card)
    (.append self.cards card)
    card)

  (defun discard (self)
    (.print-cards self)    
    (as-> (input "Pick a card:") it
          (int it) (get self.cards it)
          (progn (.remove self.cards it) it)))

  (defun print-cards (self)
    (for [i (range (len self.cards))]
      (print #f"{i}. {(get self.cards i)}"))))
 

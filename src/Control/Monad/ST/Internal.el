;; -*- lexical-binding: t; -*-

(require 'Data.Unit)

(defvar Control.Monad.ST.Internal.map_
  (lambda (f)
    (lambda (a)
      (lambda ()
         (funcall f (funcall a))))))

(defvar Control.Monad.ST.Internal.pure_
  (lambda (a)
    (lambda ()
      a)))

(defvar Control.Monad.ST.Internal.bind_
  (lambda (a)
    (lambda (f)
      (lambda ()
        (funcall (funcall f (funcall a)))))))

(defvar Control.Monad.ST.Internal.run
  (lambda (f)
    (funcall f)))

(defvar Control.Monad.ST.Internal.while
  (lambda (f)
    (lambda (a)
      (lambda ()
        (while (funcall f)
          (funcall a))
        Data.Unit.unit))))

(defvar Control.Monad.ST.Internal.for
  (lambda (lo)
    (lambda (hi)
      (lambda (f)
        (lambda ()
          (let ((i lo))
            (while (< i hi)
              (funcall (funcall f i))
              (setq i (1+ i)))
            Data.Unit.unit))))))

(defvar Control.Monad.ST.Internal.foreach
  (lambda (as)
    (lambda (f)
      (lambda ()
        (seq-do (lambda (a) (funcall (funcall f a))) as)
        Data.Unit.unit))))

(defvar Control.Monad.ST.Internal.new
  (lambda (val)
    (lambda ()
      (cons val 'Control.Monad.ST.Internal.STRef))))

(defvar Control.Monad.ST.Internal.read
  (lambda (ref)
    (lambda ()
      (car ref))))

(defvar Control.Monad.ST.Internal.modifyImpl
  (lambda (f)
    (lambda (ref)
      (lambda ()
        (let ((obj (funcall f (car ref))))
          (setcar ref (psel/alist-get 'state obj))
          (psel/alist-get 'value obj))))))

(defvar Control.Monad.ST.Internal.write
  (lambda (val)
    (lambda (ref)
      (lambda ()
        (setcar ref val)
        Data.Unit.unit))))

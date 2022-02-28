(defpackage #:lambda-macro
  (:use :cl)
  (:export #:rt))

(in-package #:lambda-macro)

(define-modify-macro maxf (&rest numbers) max)

(defun make-lambda (positional-param-count optional-param-count
                    keyword-params rest-present? allow-other-keys?
                    body
                    &key (prefix-char #\%))
  (let* ((positional (loop :for i :from 1 :upto positional-param-count
                          :collect (intern (format nil "~c~a" prefix-char i))))
         (optional (loop :for i :from 1 :upto optional-param-count
                         :collect (intern (format nil "~c?~a" prefix-char i))))
         (lambda-list `(,@positional
                        ,@(when optional
                            `(,(intern "&OPTIONAL") ,@optional))
                        ,@(when rest-present?
                            `(,(intern "&REST") ,(intern "%&")))
                        ,@(when (or keyword-params allow-other-keys?)
                            `(,(intern "&KEY") ,@keyword-params))
                        ,@(when allow-other-keys?
                            `(,(intern "&ALLOW-OTHER-KEYS")))))
         (ignore-list `(declare (ignorable ,@positional
                                           ,@optional
                                           ,@keyword-params
                                           ,@(when rest-present?
                                               `(,(intern "%&")))))))
    `(lambda ,lambda-list ,ignore-list ,body)))

(defun read-lambda (stream subchar char)
  (declare (ignore subchar char))
  (let ((*readtable* (copy-readtable))
        (positional-param-count 0) (optional-param-count 0)
        keyword-params rest-present? allow-other-keys?)
    (labels ((|read-%| (stream char)
               (declare (ignore char))
               (let ((c (peek-char nil stream t nil t)))
                 (cond
                   ((char= c #\&)
                    (read-char stream t nil t)
                    (setf rest-present? t)
                    (intern "%&"))
                   ((char= c #\?)
                    (read-char stream t nil t)
                    (let ((cn (peek-char nil stream t nil t)))
                      (unless (digit-char-p cn)
                        (error "Bad"))
                      (let ((num (read stream t nil t)))
                        (maxf optional-param-count num)
                        (intern (format nil "%?~a" num)))))
                   (t
                    (let ((data (read stream t nil t)))
                      (cond ((integerp data)
                             (maxf positional-param-count data)
                             (intern (format nil "%~a" data)))
                            ((and (symbolp data) (not (keywordp data)))
                             (push data keyword-params)
                             data)
                            (t (error "Wrong"))))))))
             (read-extra-params (c)
               (ecase c
                 (#\: (setf allow-other-keys? t))
                 (#\( (return-from read-extra-params)))
               (read-char stream t nil t)
               t))
      (set-macro-character #\% #'|read-%|)
      (loop :for c := (peek-char nil stream t nil t)
            :while (read-extra-params c))
      (let* ((body (read stream t nil t))
             (res (make-lambda positional-param-count optional-param-count
                               (nreverse keyword-params) rest-present?
                               allow-other-keys?body)))
        res))))

(named-readtables:defreadtable lambda-macro
  (:merge :standard)
  (:macro-char #\% :dispatch)
  (:dispatch-macro-char #\# #\% #'read-lambda))

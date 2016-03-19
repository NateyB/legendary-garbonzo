(defun permute (list)
  "Randomize the order of the elements in this list."
  (mapcar (lambda (pair) (rest pair))
      (sort (mapcar (lambda (item) (cons (random 1.0) item)) list)
        (lambda (a b) (> (first a) (first b))))))

(defun swap(list i j)
    (rotatef (nth i list) (nth j list)))

(defun permute-list (list &optional prefix)
    (setq list (copy-list list))
    (if (< (length list) 1)
        (list prefix)
        (let ((origPrefix (copy-list prefix)) (collection `()))
            (dotimes (count (length list))
                (setf prefix origPrefix)
                (swap list 0 count)
                (setq prefix (append prefix (list (first list))))
                (setf collection (append collection (permute-list (rest list) prefix))))
            collection)))

(defun unify(lists &optional final)
    "Given a list of sets, repeatedly applies the union operation to combine each list into one set."
    (unless final
        (setf final (first lists))
        (setf lists (rest lists)))
    (if lists
        (unify (rest lists) (mapcar `+ (first lists) final))
        final))

(defun argmax(set toMaximize &optional get-val? &aux max)
    "Using 'set', find the argument that maximizes the 'toMaximize' expression; if get-val is
    specified, find the argument and its value."
    (dolist (cur (permute set) (or (and get-val? max) (first max)))
        (let ((val (funcall toMaximize cur)))
            (if (or (not max) (> val (second max)))
                (setf max `(,cur ,val))))))

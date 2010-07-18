(eval-when-compile (require 'cl))
(require 'dict-tree)
(defvar dict-html-td nil "Dictionary dict-html-td.")
(setq dict-html-td '[cl-struct-dictree- "dict-html-td" "html/dict-html-td" t nil < + (lambda (new old) (dictree--cell-set-data old (+ (dictree--cell-data new) (dictree--cell-data old))) old) predictive-dict-rank-function (lambda (a b) (predictive-dict-rank-function (cons (car a) (dictree--cell-data (cdr a))) (cons (car b) (dictree--cell-data (cdr b))))) time synchronize nil nil nil nil nil 0.1 nil nil nil 0.1 nil nil nil nil nil nil [cl-struct-trie- [nil [cl-struct-avl-tree- [[[[[[nil nil [97 [cl-struct-avl-tree- [[[nil nil [98 [cl-struct-avl-tree- [[nil nil [98 [cl-struct-avl-tree- [[nil nil [114 [cl-struct-avl-tree- [[nil nil [--trie--terminator (0)] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] [nil nil [120 [cl-struct-avl-tree- [[nil nil [105 [cl-struct-avl-tree- [[nil nil [115 [cl-struct-avl-tree- [[nil nil [--trie--terminator (0)] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] [108 [cl-struct-avl-tree- [[nil nil [105 [cl-struct-avl-tree- [[nil nil [103 [cl-struct-avl-tree- [[nil nil [110 [cl-struct-avl-tree- [[nil nil [--trie--terminator (0)] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 1] nil nil 0] nil]] 0] nil [99 [cl-struct-avl-tree- [[nil [nil nil [111 [cl-struct-avl-tree- [[nil nil [108 [cl-struct-avl-tree- [[nil nil [115 [cl-struct-avl-tree- [[nil nil [112 [cl-struct-avl-tree- [[nil nil [97 [cl-struct-avl-tree- [[nil nil [110 [cl-struct-avl-tree- [[nil nil [--trie--terminator (0)] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] [104 [cl-struct-avl-tree- [[nil nil [97 [cl-struct-avl-tree- [[nil nil [114 [cl-struct-avl-tree- [[nil [nil nil [111 [cl-struct-avl-tree- [[nil nil [102 [cl-struct-avl-tree- [[nil nil [102 [cl-struct-avl-tree- [[nil nil [--trie--terminator (0)] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] [--trie--terminator (0)] 1] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 1] nil nil 0] nil]] 0] nil [104 [cl-struct-avl-tree- [[nil nil [101 [cl-struct-avl-tree- [[nil nil [97 [cl-struct-avl-tree- [[nil nil [100 [cl-struct-avl-tree- [[nil nil [101 [cl-struct-avl-tree- [[nil nil [114 [cl-struct-avl-tree- [[nil nil [115 [cl-struct-avl-tree- [[nil nil [--trie--terminator (0)] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil [114 [cl-struct-avl-tree- [[nil nil [111 [cl-struct-avl-tree- [[nil nil [119 [cl-struct-avl-tree- [[nil nil [115 [cl-struct-avl-tree- [[nil nil [112 [cl-struct-avl-tree- [[nil nil [97 [cl-struct-avl-tree- [[nil nil [110 [cl-struct-avl-tree- [[nil nil [--trie--terminator (0)] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] [nil nil [118 [cl-struct-avl-tree- [[nil nil [97 [cl-struct-avl-tree- [[nil nil [108 [cl-struct-avl-tree- [[nil nil [105 [cl-struct-avl-tree- [[nil nil [103 [cl-struct-avl-tree- [[nil nil [110 [cl-struct-avl-tree- [[nil nil [--trie--terminator (0)] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] [115 [cl-struct-avl-tree- [[nil nil [99 [cl-struct-avl-tree- [[nil nil [111 [cl-struct-avl-tree- [[nil nil [112 [cl-struct-avl-tree- [[nil nil [101 [cl-struct-avl-tree- [[nil nil [--trie--terminator (0)] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 0] nil nil 0] nil]] 1] nil nil 0] (lambda (a b) (setq a (trie--node-split a) b (trie--node-split b)) (cond ((eq a trie--terminator) (if (eq b trie--terminator) nil t)) ((eq b trie--terminator) nil) (t (< a b))))]] < (lambda (a b) (setq a (trie--node-split a) b (trie--node-split b)) (cond ((eq a trie--terminator) (if (eq b trie--terminator) nil t)) ((eq b trie--terminator) nil) (t (< a b)))) #[(cmpfun seq) "\301!\207" [cmpfun avl-tree-create] 2] avl-tree-enter avl-tree-delete avl-tree-member avl-tree-mapc avl-tree-empty avl-tree-stack avl-tree-stack-pop avl-tree-stack-empty-p trie--avl-transform-for-print trie--avl-transform-from-read t] nil])
(trie-transform-from-read (dictree--trie dict-html-td))
(let ((cache (make-hash-table :test 'equal))
      (trie (dictree--trie dict-html-td)))
  (mapc
   (lambda (entry)
     (puthash
      (car entry)
      (dictree--cache-create
       (mapcar
        (lambda (key)
          (cons key
                (trie-member
                 trie (if (stringp key) key (car key)))))
        (dictree--cache-results (cdr entry)))
       (dictree--cache-maxnum (cdr entry)))
      cache))
   (dictree--complete-ranked-cache dict-html-td))
  (setf (dictree--complete-ranked-cache dict-html-td)
        cache))
(let ((cache (make-hash-table :test 'equal))
      (trie (dictree--trie dict-html-td)))
  (mapc
   (lambda (entry)
     (puthash
      (car entry)
      (dictree--cache-create
       (mapcar
        (lambda (key)
          (cons key
                (trie-member
                 trie (if (stringp key) key (car key)))))
        (dictree--cache-results (cdr entry)))
       (dictree--cache-maxnum (cdr entry)))
      cache))
   (dictree--regexp-ranked-cache dict-html-td))
  (setf (dictree--regexp-ranked-cache dict-html-td)
        cache))
(unless (memq dict-html-td dictree-loaded-list)
  (push dict-html-td dictree-loaded-list))

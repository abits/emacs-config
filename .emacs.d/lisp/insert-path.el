;; insert a file path into buffer
;; by Peter Mielke
;; <http://www.rlazo.org/blog/entry/2008/sep/13/insert-a-path-into-the-current-buffer/>

(defun insert-path (file)
  "insert file"
  (interactive "FPath: ")
  (insert (expand-file-name file)))

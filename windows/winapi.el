(defconst winapi-sysdir
  "c:\\windows\\system32\\")

(defconst winapi-dlls
  '(
    "advapi32.dll"
    "comctl32.dll"
    "gdi32.dll"
    "kernel32.dll"
    "mpr.dll"
    "msvcrt.dll"
    "ole32.dll"
    "shell32.dll"
    "user32.dll"
    "wsock32.dll"
    ))

(defun winapi-line (d f)
  (concat f " stdcall " d " "))

(defun winapi ()
  (interactive)
  (with-current-buffer (get-buffer-create "*winapi*")
    (let ((buffer-undo-list t))
      (erase-buffer)
      (dolist (d winapi-dlls)
        (let ((x (point))) 
          (insert (shell-command-to-string
                   (concat
                    "objdump -x "
                    (shell-quote-argument (concat winapi-sysdir d)))))
          (goto-char x)
          (search-forward "[Ordinal/Name Pointer] Table\n")
          (delete-region x (point))
          (while (looking-at "^\t\\[ *[0-9]+\\] \\(.+\\)$")
            (replace-match (winapi-line d (match-string 1)) t t)
            (forward-line 1)
            (goto-char (point-at-bol)))
          (delete-region (point) (point-max))))
      (let ((sort-fold-case t)) (sort-lines nil (point-min) (point-max)))
      (message "winapi done"))))

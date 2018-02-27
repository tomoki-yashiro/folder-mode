;;; -*- emacs-lisp -*-

(defvar folder-debug nil
  "* if non-nil show debug message.")

(eval-when-compile
  (defvar folder-obarray nil)
  (defvar folder-move-direction nil)
  (defvar folder-get-children-function nil)
  (defvar folder-info-function nil)
  (defvar folder-process-function nil)
  (defvar folder-get-children-functions nil)
  (defvar folder-info-functions nil)
  (defvar folder-process-functions nil))

(defvar folder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" 'folder-open)
    (define-key map "c" 'folder-close)
    (define-key map "h" 'folder-toggle-hide)
    (define-key map "n" 'folder-next)
    (define-key map "p" 'folder-previous)
    (define-key map "N" 'folder-next-sibling)
    (define-key map "P" 'folder-previous-sibling)
    (define-key map "^" 'folder-goto-parent)
    (define-key map "<" 'folder-goto-parent)

    (define-key map "*" 'folder-mark)
    (define-key map "u" 'folder-unmark)
    (define-key map "mn" 'folder-mark-next)
    (define-key map "mp" 'folder-mark-previous)
    (define-key map "mu" 'folder-mark-unmark)

    (define-key map "r" 'folder-update)
    (define-key map "R" 'folder-update-all)

    (define-key map [return] 'folder-process)

    (define-key map "sc" 'folder-select-children-function)
    (define-key map "si" 'folder-select-info-function)
    (define-key map "sp" 'folder-select-process-function)

    (define-key map [mouse-1] 'folder-mouse-goto)

    map))

(defvar folder-mode-mouse-mark-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'folder-process-mouse)

    (set-keymap-parent map folder-mode-map)
    map))

(defconst folder-marks
  (let ((marks '("[+]" "[-]" "<->")))
    (mapc (lambda (m)
            (put-text-property 0 3
                               'mouse-face '((background-color . "green"))
                               m)
            (put-text-property 0 2 'local-map folder-mode-mouse-mark-map m))
          marks)
    (list (cons 'close (nth 0 marks))
          (cons 'open  (nth 1 marks))
          (cons 'hide  (nth 2 marks))
          (cons 'file  " - "))))

(defvar folder-image-marks
  (if (member 'xpm image-types)
      (let ((spec '(:type xpm :ascent center))
            image-close image-open image-hide image-file)

        (plist-put spec :file "ezimage/dir-plus.xpm")
        (setq image-close (find-image (list spec)))

        (plist-put spec :file "ezimage/dir-minus.xpm")
        (setq image-open (find-image (list spec)))

        (plist-put spec :file "ezimage/dir.xpm")
        (setq image-hide (find-image (list spec)))

        (plist-put spec :file "ezimage/page.xpm")
        (setq image-file (find-image (list spec)))

        (list (cons 'close image-close)
              (cons 'open  image-open)
              (cons 'hide  image-hide)
              (cons 'file  image-file)))))

(defvar folder-use-image t
  "* if non-nil use image.")

(defvar folder-open-folder-hook nil)
(defvar folder-close-folder-hook nil)
(defvar folder-select-hook nil)

;;; symbol 関連
(defun folder-regist (path)
  (intern path folder-obarray))

(defun folder-unregist (path)
  (unintern path folder-obarray))

(defun folder-symbol (path)
  (intern-soft path folder-obarray))

(defun folder-status (sym)
  (get sym 'folder-status))
(defun folder-mark-position (sym)
  (get sym 'folder-mark-position))
(defun folder-cursor-position (sym)
  (get sym 'folder-cursor-position))
(defun folder-end-position (sym)
  (get sym 'folder-end-position))
(defun folder-parent (sym)
  (get sym 'folder-parent))
(defun folder-children (sym)
  (get sym 'folder-children))
(defun folder-child-branch (sym)
  (get sym 'folder-child-branch))
(defun folder-invisible (sym)
  (get sym 'folder-invisible))

(defun folder-set-status (sym status)
  (or sym (error "folder-set-status sym is nil"))
  (put sym 'folder-status status))
(defun folder-set-mark-position (sym pos)
  (or sym (error "folder-set-mark-position sym is nil"))
  (put sym 'folder-mark-position pos))
(defun folder-set-cursor-position (sym pos)
  (or sym (error "folder-set-cursor-position sym is nil"))
  (put sym 'folder-cursor-position pos))
(defun folder-set-end-position (sym pos)
  (or sym (error "folder-set-end-position sym is nil"))
  (put sym 'folder-end-position pos))
(defun folder-set-parent (sym parent)
  (or sym (error "folder-set-parent sym is nil"))
  (put sym 'folder-parent parent))
(defun folder-add-children (sym child)
  (or sym (error "folder-add-children sym is nil"))
  (put sym 'folder-children (cons child (get sym 'folder-children))))
(defun folder-clear-children (sym)
  (or sym (error "folder-clear-children sym is nil"))
  (put sym 'folder-children nil))
(defun folder-set-child-branch (sym branch)
  (or sym (error "folder-set-child-branch sym is nil"))
  (put sym 'folder-child-branch branch))
(defun folder-set-invisible (sym invisible)
  (or sym (error "folder-set-invisible sym is nil"))
  (put sym 'folder-invisible invisible))

(defun folder-delete-child-sym (sym)
  (let ((children (folder-children sym)))
    (while children
      ;;; さらに子がある場合は先に削除しておく
      (folder-delete-child-sym (car children))

      ;;; 子に対する処理
      (setq buffer-invisibility-spec
            (delete (cons (car children) t) buffer-invisibility-spec))
      (folder-unregist (car children))

      (setq children (cdr children)))))

;;;;;
(defun folder-regist-info (info)
  (folder-make-info
   (folder-regist (folder-info-id info))
   (folder-info-folder-p info)
   (folder-info-name info)
   (folder-info-extension info)))

(defun folder-get-children (parent)
  (mapcar (lambda (info)
            (prog1
                (setq info (folder-regist-info info))
              (let ((sym (folder-info-id info)))
                (folder-set-parent sym parent)
                (folder-add-children parent sym))))
          (funcall folder-get-children-function (symbol-name parent))))

(defun folder-info (sym)
  (funcall folder-info-function (symbol-name sym)))

(defun folder-process-file (sym)
  (funcall folder-process-function (symbol-name sym)))

(defun folder-make-info (id folder-p name &optional extension)
  (list id folder-p name extension))

(defun folder-info-id (info)
  (nth 0 info))

(defun folder-info-folder-p (info)
  (nth 1 info))

(defun folder-info-name (info)
  (nth 2 info))

(defun folder-info-extension (info)
  (nth 3 info))

;;(defun folder-add-children-function (&rest funcs)
;;  (setq folder-get-children-functions
;;        (append folder-get-children-functions funcs)))

;;(defun folder-add-info-function (&rest funcs)
;;  (setq folder-info-functions
;;        (append folder-info-functions funcs)))

;;(defun folder-add-process-function (&rest funcs)
;;  (setq folder-process-functions
;;        (append folder-process-functions funcs)))

(defun folder-select-function (funcs)
  (intern-soft
   (completing-read
    "Function: "
    (mapcar (lambda (func) (list (symbol-name func))) funcs)
    nil t)))

(defun folder-select-children-function (func)
  (interactive (list (folder-select-function folder-get-children-functions)))
  (setq folder-get-children-function func)
  (folder-update-all))

(defun folder-select-info-function (func)
  (interactive (list (folder-select-function folder-info-functions)))
  (setq folder-info-function func)
  (folder-update-all))

(defun folder-select-process-function (func)
  (interactive (list (folder-select-function folder-process-functions)))
  (setq folder-process-function func)
  (folder-update-all))

(defun folder-truncate-string (str width &optional left)
  (let ((len (length str)))
    (cond
     ((= width len) str)
     ((< width len)
      (let ((original str))
        (setq str
              (if left
                  (concat "..." (substring str (+ 3 (- len width))))
                (concat (substring str 0 (- width 3)) "...")))
        (put-text-property 0 width 'help-echo original str)
        str))
     (t
      (if left
          (concat (make-string (- width len) ?\x20) str)
        (truncate-string-to-width str width 0 ?\x20))))))

;;;;;
(defun folder-update-mark (sym)
  (let ((mark-pos (folder-mark-position sym))
        (cursor-pos (folder-cursor-position sym)))
    (let ((props (text-properties-at cursor-pos))
          (status (folder-status sym))
          (buffer-read-only nil))
      (delete-region mark-pos cursor-pos)
      (goto-char mark-pos)
      (let ((image (if folder-use-image
                       (assoc-default status folder-image-marks))))
        (if image
            (progn
              (insert-image image)
              (setq props (append props
                                  (list 'intangible image 'display image))))
          (insert (assoc-default status folder-marks)))

        (add-text-properties mark-pos cursor-pos props)
        ))))

(defun folder-change-status (sym status)
  (folder-set-status sym status)
  (folder-update-mark sym))

(defun folder-current-sym (&optional pos)
  (get-text-property (or pos (point)) 'folder-sym))

(defun folder-sibling (sym direction)
  (unless (= direction 0)
    (let ((children (folder-children (folder-parent sym)))
          prev next)
      (while children
        (if (eq sym (car children))
            (setq next (cadr children)
                  children nil)
          (setq prev (car children)
                children (cdr children))))
      (if (< direction 0) next prev))))

(defun folder-marked-syms ()
  (save-excursion
    (widen)
    (goto-char (point-min))
    (let (val)
      (while (re-search-forward "^\*" nil t)
        (setq val (cons (folder-current-sym) val)))
      val)))

(defun folder-invisible-p (point)
  (let ((invisible (get-text-property point 'invisible)))
    (while (and invisible
                ;; (not (memq (car invisible) buffer-invisibility-spec))
                (not (assoc (car invisible) buffer-invisibility-spec)))
      (setq invisible (cdr invisible)))
    invisible))

(defun folder-forward-line (n)
  (setq folder-move-direction (if (< n 0) -1 1))
  (while (and (not (zerop n))
              (folder-current-sym
               (line-beginning-position (1+ folder-move-direction)))
              (zerop (forward-line folder-move-direction)))
    (or (folder-invisible-p (point))
        (setq n (- n folder-move-direction))))
  n)

(defun folder-jump-no-select (sym)
  (goto-char (folder-cursor-position sym)))

(defun folder-setup (top
                     get-children-funcs
                     info-funcs
                     process-funcs)
  (set (make-local-variable 'folder-get-children-functions) get-children-funcs)
  (set (make-local-variable 'folder-info-functions) info-funcs)
  (set (make-local-variable 'folder-process-functions) process-funcs)
  (set (make-local-variable 'folder-get-children-function) (car get-children-funcs))
  (set (make-local-variable 'folder-info-function) (car info-funcs))
  (set (make-local-variable 'folder-process-function) (car process-funcs))

  ;;
  (set (make-local-variable 'folder-obarray) (make-vector 3 0))
  (set (make-local-variable 'show-paren-mode) nil)
  (or (listp buffer-invisibility-spec)
      (setq buffer-invisibility-spec nil))

  (setq top (folder-regist-info top))

  (setq buffer-read-only nil)
  (save-excursion
    (save-excursion
      (insert "\n"))
    (folder-insert top "" nil))
  (delete-char 1)
  (setq buffer-read-only t)

  (let ((sym (folder-info-id top)))
    (folder-set-child-branch sym "   ")
    (folder-jump sym)))

(defun folder-add-properties (str properties)
  (while properties
    (or (next-single-property-change 0 (car properties) str)
        (put-text-property 0 (length str)
                           (car properties) (cadr properties) str))
    (setq properties (cddr properties)))
  str)

(defun folder-insert (info branch invisible)
  (let ((sym (folder-info-id info))
        (folder-p (folder-info-folder-p info))
        buffer-read-only marker-pos cursor-pos end-pos)

    (insert-and-inherit "\n")
    (insert " " (folder-info sym) branch)
    (setq marker-pos (point-marker))
    (insert " ")                        ;;; dummy mark string
    (setq cursor-pos (point-marker))
    (insert " " (folder-info-name info))
    (setq end-pos (point-marker))

    (put-text-property (line-beginning-position 1)
                       (line-beginning-position 2)
                       'folder-sym sym)
    (put-text-property (line-end-position 0)
                       (line-end-position 1)
                       'invisible invisible)

    (set-marker-insertion-type cursor-pos t)
    (set-marker-insertion-type end-pos t)

    (folder-set-mark-position sym marker-pos)
    (folder-set-cursor-position sym cursor-pos)
    (folder-set-end-position sym end-pos)

    (if folder-p
        (folder-set-child-branch sym (concat branch " | ")))
    (folder-change-status sym (if folder-p 'close 'file))
    (folder-clear-children sym)))

(defun folder-mark-sym (sym mark-p)
  (let (buffer-read-only)
    (save-excursion
      (folder-jump-no-select sym)
      (beginning-of-line)
      (forward-char 1)
      (insert-char (if mark-p ?\* ?\x20) 1 t)
      (backward-char 2)
      (delete-char 1))))

;;;; 外部パッケージに対するインターフェース

(defun folder-current-path (&optional pos)
  (let ((sym (folder-current-sym pos)))
    (if sym (symbol-name sym))))

(defun folder-current-folder (&optional pos)
  (let ((sym (folder-current-sym pos)))
    (and sym
         (or (not (eq 'file (folder-status sym)))
             (setq sym (folder-parent sym)))
         (symbol-name sym))))

(defun folder-marked-path ()
  (mapcar 'symbol-name (folder-marked-syms)))

(defun folder-get-path ()
  (or (folder-marked-path)
      (folder-current-path)))

(defun folder-top-p (path)
  (let ((sym (folder-symbol path)))
    (eq nil (folder-parent sym))))

(defun folder-goto-path (path)
  (folder-jump (folder-symbol path)))

(defun folder-open-path (path)
  (let ((sym (folder-symbol path)))
    (if sym
        (folder-open sym))))

(defun folder-exist-p (path)
  (folder-symbol path))

(defun folder-update-path (paths)
  (if folder-debug
      (message "folder-update-path(%s)" paths))

  (mapc (lambda (path)
          (let ((sym (folder-symbol path)))
            (if sym
                (folder-update sym)))) paths))

;;;;; interactive

(defun folder-jump (&optional sym)
  (interactive (list (folder-symbol
                      (completing-read "Go to: " folder-obarray))))

  (when (or sym (setq sym (folder-current-sym)))
    (folder-jump-no-select sym)
    (run-hook-with-args 'folder-select-hook (folder-symbol sym))))

(defun folder-open (&optional sym arg)
  (interactive)
  (or sym
      (progn
        (if (folder-invisible-p (point))
            (folder-previous 1))
        (setq sym (folder-current-sym))))
  (or (numberp arg)
      (setq arg (prefix-numeric-value current-prefix-arg)))

  (let (status action)
    (setq status (folder-status sym)
          action
          (cond
           ((eq status 'file) nil)
           ((> arg 0) (if (eq status 'close) 'open nil))
           ((< arg 0) (if (eq status 'close) nil 'close))
           ((= arg 0) (cond
                       ((eq status 'open) 'close)
                       ((eq status 'hide) 'close)
                       ((eq status 'close) 'open)
                       (t nil)))))
    (cond
     ((eq action 'open)
      (let ((children (reverse (folder-get-children sym)))
            (branch (folder-child-branch sym))
            (invisible (cons sym
                             (get-text-property (folder-cursor-position sym)
                                                'invisible)))
            last)

        (folder-change-status sym 'open)
        (setq last (folder-info-id (car children)))

        (save-excursion
          (goto-char (folder-end-position sym))
          (while children
            (save-excursion
              (folder-insert (car children) branch invisible))
            (setq children (cdr children))))

          ;;; 最後だけ設定しなおす
        (or (not last)
            (eq (folder-status last) 'file)
            (folder-set-child-branch last (concat branch "   ")))

        (run-hook-with-args 'folder-open-folder-hook (symbol-name sym))))

     ((eq action 'close)
      (save-excursion
        (folder-jump sym)
        (let (buffer-read-only)
          (delete-region (line-end-position 1) (folder-end-position sym))
          (put-text-property (line-beginning-position 1)
                             (line-beginning-position 2)
                             'folder-sym sym))
        (remove-from-invisibility-spec (cons sym t))
        (folder-delete-child-sym sym)
        (folder-clear-children sym)

        (folder-change-status sym 'close))
      (folder-jump sym)

      (run-hook-with-args 'folder-close-folder-hook (symbol-name sym))))))

(defun folder-close (&optional sym arg)
  (interactive)
  (folder-open sym (- (prefix-numeric-value arg))))

(defun folder-toggle-hide (&optional arg)
  ;; arg > 0   -> hide
  ;; arg <= 0  -> show
  (interactive "P")
  (if (folder-invisible-p (point))
      (folder-previous 1))

  (let* ((sym (folder-current-sym))
         (status (folder-status sym)))
    (cond
     ((and (eq status 'open)
           (or (not arg) (< 0 arg)))
      (add-to-invisibility-spec (cons sym t))
      (folder-change-status sym 'hide))

     ((and (eq status 'hide)
           (or (not arg) (< arg 0)))
      (remove-from-invisibility-spec (cons sym t))
      (folder-change-status sym 'open))))
  (redraw-display))

(defun folder-goto-parent (n)
  (interactive "p")
  (if (folder-invisible-p (point))
      (folder-previous 1))

  (let (parent)
    (while (< 0 n)
      (setq parent (folder-parent (folder-current-sym))
            n (1- n)))
    (if parent
        (folder-jump parent)))
  n)

(defun folder-next (n)
  (interactive "p")
  (prog1
      (folder-forward-line n)
    (folder-jump)))

(defun folder-previous (n)
  (interactive "p")
  (folder-next (- n)))

(defun folder-next-sibling (n)
  "* 次の兄弟ファイルに移動する。"
  (interactive "p")
  (setq folder-move-direction (if (< 0 n) 1 -1))
  (let ((sibling nil)
        (sym (folder-current-sym)))
    (while (and (not (zerop n))
                (setq sibling
                      (folder-sibling sym folder-move-direction)))
      (setq n (- n folder-move-direction)
            sym sibling))
    (if sym
        (folder-jump sym)))
  n)

(defun folder-previous-sibling (n)
  "* 前の兄弟ファイルに移動する。"
  (interactive "p")
  (- (folder-next-sibling (- n))))

(defun folder-mark (n)
  (interactive "p")
  (if (folder-invisible-p (point))
      (folder-previous 1))
  (let ((mark-p (<= 0 n)))
    (setq n (abs n))
    (while (and (< 0 n)
                (progn
                  (folder-mark-sym (folder-current-sym) mark-p)
                  (setq n (1- n))
                  (zerop (folder-forward-line folder-move-direction)))))
    (folder-jump))
  n)

(defun folder-unmark (n)
  (interactive "p")
  (if (folder-invisible-p (point))
      (folder-previous 1))
  (folder-mark (- n)))

(defun folder-mark-next (n)
  (interactive "p")
  (end-of-line)
  (while (and (< 0 n)
              (re-search-forward "^\*" nil t))
    (setq n (1- n)))
  (folder-jump)
  n)

(defun folder-mark-previous (n)
  (interactive "p")
  (if (folder-invisible-p (point))
      (folder-previous 1))
  (beginning-of-line)
  (while (and (< 0 n)
              (re-search-backward "^\*" nil t))
    (setq n (1- n)))
  (folder-jump)
  n)

(defun folder-mark-unmark ()
  (interactive)
  (mapcar (lambda (sym) (folder-mark-sym sym nil))
          (folder-marked-syms)))

(defun folder-update (&optional sym)
  (interactive)
  (or sym
      (setq sym (folder-current-sym)))
  (save-excursion
    (folder-jump-no-select sym)
    (beginning-of-line 1)
    (let ((buffer-read-only)
          (info (concat (folder-info sym)
                (folder-child-branch (folder-parent sym))))
          (props (text-properties-at (point))))
      (forward-char 1)
      (insert-before-markers (folder-add-properties info props))
      (delete-region (point)
                     (folder-mark-position sym)))))

(defun folder-update-all ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (sym)
        (while (progn
                 (if (setq sym (folder-current-sym))
                     (folder-update sym))
                 (zerop (forward-line))))))))

(defun folder-process (&optional arg)
  (interactive "P")
  (if (folder-invisible-p (point))
      (folder-previous 1))
  (let* ((sym (folder-current-sym))
         (status (folder-status sym)))
    (cond
     ((eq status 'close)
      (folder-open sym))
     ((eq status 'open)
      (if arg
          (folder-toggle-hide)
        (folder-close)))
     ((eq status 'hide)
      (folder-toggle-hide))
     ((eq status 'file)
      (folder-process-file sym))
     (t nil))))

(defun folder-process-mouse (event)
  (interactive "e")
  (mouse-minibuffer-check event)
  (select-window (posn-window (event-start event)))
  (save-excursion
    (goto-char (posn-point (event-end event)))
    (if (folder-current-sym)
        (folder-process))))

(defun folder-mouse-goto (event)
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (folder-jump (folder-current-sym))
  (if (folder-invisible-p (point))
      (folder-previous 1)))

(provide 'folder-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

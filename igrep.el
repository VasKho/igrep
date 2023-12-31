;;; igrep --- Package to interactively grep current buffer directory
;;;
;;; Author: vaslch0 <vasya.khoroshavin@gmail.com>
;;; URL: https://github.com/VasKho/igrep
;;; Package-Requires: (cl-lib json posframe)
;;; Commentary:
;;; Provides simple interface to interactively grep files in current buffer directory
;;; Code:

(require 'posframe)
(require 'json)
(require 'cl-lib)

(defvar igrep--input-buffer "*igrep-input*")
(defvar igrep--candidate-buffer "*igrep-candidate*")
(defvar igrep--result-buffer "*igrep-result*")
(defvar igrep--return-buffer "")
(defvar igrep--result-window "")
(defvar igrep--search-results nil)
(defvar igrep--start-flag nil)


(defun igrep--view-file-at-line (file line)
  "View FILE on LINE."
  (select-window igrep--result-window)
  (when (not (eq (current-buffer) igrep--return-buffer))
    (kill-current-buffer))
  (view-file file)
  (forward-line line)
  (select-window (get-buffer-window igrep--input-buffer)))

(defun igrep--listen-input (begin end length)
  "Start search when `igrep--input-buffer' content is changed.
Accepts BEGIN, END and LENGTH, but this info isn't used."
  (when (string-equal (buffer-name) igrep--input-buffer)
    (let ((input
	   (string-trim
            (with-current-buffer igrep--input-buffer
              (buffer-substring-no-properties (point-min) (point-max))))))
      (when (length> input 2)
	(igrep--parse-results (shell-command-to-string (format "rg -H --json %S %s" input default-directory)))))))

(defun igrep--show-posframe (buffer)
  "Show posframe with BUFFER as `igrep--input-buffer'."
  (let* ((posframe-height (round (* (frame-height) 0.7)))
         (posframe-width (round (* (frame-width) 0.7))))
    (apply #'posframe-show
           (get-buffer buffer)
           :poshandler #'posframe-poshandler-frame-center
           (list
            :max-height posframe-height
            :min-height posframe-height
            :min-width  posframe-width
            :max-width  posframe-width
            :border-width 2
            :border-color "gray"
	    :accept-focus (equal buffer igrep--input-buffer)))))

(defun igrep--init-layout ()
  "This function is used to build layout of output posframe."
  (let ((igrep-posframe-emacs-frame (selected-frame))
	(igrep-posframe (igrep--show-posframe igrep--input-buffer)))
    
    ;; Make popup frame's font same as Emacs frame one.
    (with-selected-frame igrep-posframe
      (set-frame-font
       (with-selected-frame igrep-posframe-emacs-frame
	 (face-attribute 'default :font))))

    (select-frame-set-input-focus igrep-posframe)
    (split-window (selected-window) (line-pixel-height) 'below t)

    (other-window 1)
    (split-window (selected-window) nil 'right t)
    (switch-to-buffer igrep--candidate-buffer)

    (other-window 1)
    (switch-to-buffer igrep--result-buffer)

    (setq igrep--result-window (get-buffer-window igrep--result-buffer))
    (select-window (get-buffer-window igrep--input-buffer))
    (setq-local cursor-type 'box)
    (set-window-margins (get-buffer-window igrep--input-buffer) 1 1)))

;;;###autoload
(defun igrep ()
  "This function is used to start igrep search in current buffer directory."
  (interactive)
  (setq igrep--return-buffer (current-buffer))
  (with-current-buffer (get-buffer-create igrep--input-buffer)
    (erase-buffer)
    (igrep-mode)
    (setq-local left-margin-width 1)
    (setq-local right-margin-width 1))

  (with-current-buffer (get-buffer-create igrep--result-buffer)
    (read-only-mode 0)
    (erase-buffer)
    (setq-local left-margin-width 1)
    (setq-local right-margin-width 1)
    (setq-local mode-line-format nil)
    (read-only-mode 1))

  (with-current-buffer (get-buffer-create igrep--candidate-buffer)
    (read-only-mode 0)
    (erase-buffer)
    (setq-local truncate-lines t)
    (setq-local mode-line-format nil)
    (read-only-mode 1))

  (igrep--init-layout)
  (add-hook 'after-change-functions 'igrep--listen-input nil t))

(defun igrep--parse-results (input)
  "This function is used to split search results and insert them into buffer.
INPUT is a search result in ripgrep json format."
  (let* ((input (split-string input "\n"))
	 (len (- (length input) 1))
	 (ind 0)
	 (path "")
	 (line ""))
    (with-current-buffer (get-buffer-create igrep--candidate-buffer)
      (setq igrep--search-results nil)
      (read-only-mode 0)
      (erase-buffer)
      (dolist (line input)
	(when (< ind (- len 1))
	  (cl-incf ind 1)
	  (setq line (json-parse-string line :object-type 'plist))
	  (when (string-equal (plist-get line :type) "match")
	    (setq path (plist-get (plist-get (plist-get line :data) :path) :text))
	    (setq line (plist-get (plist-get line :data) :line_number))
	    (insert (format "%s: %s\n" (file-name-nondirectory path) line))
	    (push `(:path ,path :line ,line) igrep--search-results))))
      (read-only-mode 1)
      (goto-char (point-min)))
    (setq igrep--start-flag t)
    (setq igrep--search-results (reverse igrep--search-results))))

;;;###autoload
(defun igrep-quit ()
  "Close posframe with search reults."
  (interactive)
  (select-window igrep--result-window)
  (when (not (eq (current-buffer) igrep--return-buffer))
    (kill-current-buffer))
  (kill-buffer igrep--candidate-buffer)
  (when (get-buffer igrep--result-buffer)
    (kill-buffer igrep--result-buffer))
  (setq igrep--result-window "")
  (setq igrep--search-results nil)
  (select-window (get-buffer-window igrep--input-buffer))
  (posframe-delete-all)
  (switch-to-buffer igrep--return-buffer)
  (read-only-mode 0)
  (setq igrep--return-buffer nil))

;;;###autoload
(defun igrep-select-next-candidate ()
  "This function is used to select next candidate in search results."
  (interactive)
  (with-current-buffer (get-buffer-create igrep--candidate-buffer)
    (let* ((prev-line (string-to-number (nth 1 (split-string (what-line) " "))))
	   (prev-info (nth (- prev-line 1) igrep--search-results))
	   (current-line nil)
	   (current-info nil))
      (if (not igrep--start-flag)
	  (when (not (= (count-lines (point-min) (point-max)) prev-line))
	    (forward-line 1)
	    (setq current-line (string-to-number (nth 1 (split-string (what-line) " "))))
	    (setq current-info (nth (- current-line 1) igrep--search-results))
	    (if (string-equal (plist-get prev-info :path) (plist-get current-info :path))
		(progn
		  (select-window igrep--result-window)
		  (goto-char (point-min))
		  (forward-line (plist-get current-info :line))
		  (select-window (get-buffer-window igrep--input-buffer)))
	      (igrep--view-file-at-line (plist-get current-info :path) (plist-get current-info :line))))
	(progn
	  (setq igrep--start-flag nil)
	  (igrep--view-file-at-line (plist-get prev-info :path) (plist-get prev-info :line)))))))

;;;###autoload
(defun igrep-select-prev-candidate ()
  "This function is used to select previous candidate in search results."
  (interactive)
  (with-current-buffer (get-buffer-create igrep--candidate-buffer)
    (let* ((current-line (string-to-number (nth 1 (split-string (what-line) " "))))
	   (current-info (nth (- current-line 1) igrep--search-results))
	   (prev-line nil)
	   (prev-info nil))
      (forward-line -1)
      (setq prev-line (string-to-number (nth 1 (split-string (what-line) " "))))
      (setq prev-info (nth (- prev-line 1) igrep--search-results))
      (if (string-equal (plist-get prev-info :path) (plist-get current-info :path))
	  (progn
	    (select-window igrep--result-window)
	    (goto-char (point-min))
	    (forward-line (plist-get prev-info :line))
	    (select-window (get-buffer-window igrep--input-buffer)))
	(igrep--view-file-at-line (plist-get prev-info :path) (plist-get prev-info :line))))))

;;;###autoload
(defun igrep-open-file-at-line ()
  "Open FILE on LINE."
  (interactive)
  (with-current-buffer (get-buffer-create igrep--candidate-buffer)
    (let* ((current-line (string-to-number (nth 1 (split-string (what-line) " "))))
	   (current-info (nth (- current-line 1) igrep--search-results)))
      (igrep-quit)
      (find-file (plist-get current-info :path))
      (forward-line (1- (plist-get current-info :line))))))

(defvar igrep-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'igrep-quit)
    (define-key map (kbd "ESC ESC ESC") 'igrep-quit)
    (define-key map (kbd "C-n") 'igrep-select-next-candidate)
    (define-key map (kbd "TAB") 'igrep-select-next-candidate)
    (define-key map (kbd "C-p") 'igrep-select-prev-candidate)
    (define-key map (kbd "<backtab>") 'igrep-select-prev-candidate)
    (define-key map (kbd "RET") 'igrep-open-file-at-line)
    map)
  "Keymap used by `igrep-mode'.")

(define-derived-mode igrep-mode text-mode "igrep"
  (kill-all-local-variables)
  (setq major-mode 'igrep-mode)
  (setq mode-name "igrep")
  (use-local-map igrep-mode-map))

(provide 'igrep)
;;; igrep.el ends here

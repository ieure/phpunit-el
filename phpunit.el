;;; phpunit.el --- Interact with PHPUnit

;; Copyright © 2008, 2009 Ian Eure
;; $Id$

;; Maintainer: Ian Eure <ian.eure@gmail.com>
;; Author: Ian Eure
;; Keywords: php phpunit xunit tests
;; Created: 2008-12-01
;; Modified: 2007-01-02
;; X-URL:   http://atomized.org

;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Usage

;; Put this file in your Emacs lisp path (eg. site-lisp) and add to
;; your .emacs file:
;;
;;   (require 'phpunit)
;;
;; You can run a test by invoking C-c C-l t (phpunit-run-test)
;;
;; If you customize 'phpunit-testable-list, a corresponding test file
;; will be searched for when you open a .php file.

(defconst phpunit-version-number "0.8.6"
  "PHPUnit Mode version number.")

(require 'compile)

(defgroup phpunit nil
  "Minor mode for running PHPUnit tests."
  :prefix "phpunit-"
  :group 'php)

;;;###autoload
(defcustom phpunit-testable-list
  '(("modules/.*\.php$"
         (lambda (file)
           "Get test class from FILE"
           (subst-char-in-string ?/ ?_
                                 (replace-regexp-in-string
                                  "^.*/modules/[^/]+/\\(tests/\\)?\\(.*\\)?\.php$" "\\2Test" (replace-regexp-in-string "\\(Test\\)?\.php$" ".php" file t))))

         (lambda (file)
           "Get test file from FILE"
           (replace-regexp-in-string
            "^\\(.*/modules/[^/]+\\)\\(.*\\)" "\\1/tests\\2Test.php"
            (replace-regexp-in-string "\\(tests/\\|\\(Test\\)?\.php$\\)" "" file)))))
  "List of files which are testable, path to the unit test file, and test file class name.

Each elt has the form (EXP CLASS FILE).

EXP is a regular expression matching a visited file.
CLASS is a string or function for the class name of the PHPUnit test class.
FILE is a string or function for the file name of the PHPUnit test file."
  :type 'list
  :group 'phpunit)

;;;###autoload
(defcustom phpunit-program
  "phpunit"
  "Default PHPUnit program"
  :type 'string
  :group 'phpunit)

;;;###autoload
(defcustom phpunit-args
  ""
  "Default arguments to PHPUnit"
  :type 'string
  :group 'phpunit)

(defvar phpunit-command (format "%s %s" phpunit-program phpunit-args))

;;;###autoload
(defcustom phpunit-setup-hook nil
  "List of hook functions run by `phpunit-process-setup' (see `run-hooks')."
  :type 'hook
  :group 'phpunit)

(defvar phpunit-history nil)

;;;###autoload
(defvar phpunit-regexp-alist
  '(("^\\(.*\\.php\\):\\([0-9]+\\)$" 1 2 nil nil 1))
  "Regexp used to match PHPUnit output. See `compilation-error-regexp-alist'.")

;;;###autoload
(defvar phpunit-error-face compilation-error-face
  "Face name to use for phpunit errors.")

;;;###autoload
(defvar phpunit-warning-face compilation-warning-face
  "Face name to use for phpunit warnings.")

;;;###autoload
(define-minor-mode phpunit-minor-mode
  "Minor mode for running PHPUnit tests" nil " Test"
  (make-sparse-keymap)
  :group 'phpunit
  :global nil)
(define-key phpunit-minor-mode-map "\C-c\C-lr" 'phpunit-run-test-or-retest)
(define-key phpunit-minor-mode-map "\C-c\C-lt" 'phpunit-test-this)
(define-key phpunit-minor-mode-map "\C-c\C-lf" 'phpunit-find-test-file)

;;;###autoload
(define-compilation-mode phpunit-run-mode "PHPUnit"
  (set (make-local-variable 'compilation-error-regexp-alist)
       phpunit-regexp-alist)
  (set (make-local-variable 'compilation-error-face)
       phpunit-error-face)
  (set (make-local-variable 'compilation-warning-face)
       phpunit-warning-face)
  (set (make-local-variable 'compilation-process-setup-function)
       'phpunit-process-setup)
  (set (make-local-variable 'compilation-disable-input) t))
(define-key phpunit-run-mode-map "R" 'phpunit-run-test-or-retest)

;;;###autoload
(defun phpunit-process-setup ()
  "Setup compilation variables and buffer for `phpunit'.
Run `phpunit-setup-hook'."
  (run-hooks 'phpunit-setup-hook))

;; (defmacro phpunit-getinfo (file class &rest body)
;;   "Get "
;;   `(let* ((file (or ,file (phpunit-test-file-name)))
;;          (,class (or ,class (phpunit-test-class-name ,file))))
;;     ,@body))

;;;###autoload
(defun phpunit-read-command (command)
  "Read a PHPUnit command to execute."
  (read-shell-command "PHPUnit command: " command
                      (if (equal (car phpunit-history) command)
                          '(phpunit-history . 1)
                        'phpunit-history)))

;;;###autoload
(defun phpunit-run-test (command &optional comint)
  "Runs a test with PHPUnit."
  (interactive
   (list
    (let ((command phpunit-command))
      (phpunit-read-command command))))
  (save-some-buffers t)
  (unless (equal command (eval phpunit-command))
    (setq phpunit-command command))
  (setq-default phpunit-run-directory default-directory)
  (compilation-start command 'phpunit-run-mode))

;;;###autoload
(defun phpunit-run-test-or-retest ()
  "Re-run the last PHPUnit test (if any), or start a new test."
  (interactive)
  (or (and phpunit-history
           (let ((default-directory (or phpunit-run-directory
                                        default-directory)))
             (phpunit-run-test (car phpunit-history))))
      (call-interactively 'phpunit-run-test)))

;;;###autoload
(defun phpunit-test-this (&optional file class)
  "Performs a PHPUnit test of CLASS, which exists in FILE."
  (interactive)
  (let* ((file (or file (phpunit-test-file-name)))
         (real-file (or (and (tramp-tramp-file-p file)
                             (aref (tramp-dissect-file-name file) 3)) file))
         (class (or class (phpunit-test-class-name file)))
         (phpunit-program
          (or (and (tramp-tramp-file-p phpunit-program)
                   (aref (tramp-dissect-file-name phpunit-program) 3))
              phpunit-program)))
    (or (and (phpunit-testablep file)
             (phpunit-run-test (format "%s %s %s %s" phpunit-program phpunit-args class real-file)))
        (message "This isn't testable"))))

;;;###autoload
(defun phpunit-testablep (&optional file)
  "Non-nil if FILE is testable with PHPUnit.
If FILE is omitted, the currently visited file is used."
  (or (bound-and-true-p phpunit-testable)
      (let* ((file (or file (buffer-file-name)))
             (match (phpunit-get-match file)))
        (and match (file-exists-p (funcall (caddr match) file))))))

;;;###autoload
(defun phpunit-get-match (&optional file)
  "Return the set from `phpunit-testable-list' which matches FILE.
Returns `nil' if FILE doesn't match any patterns in `phpunit-testable-list'"
  (let ((file (or file (buffer-file-name)))
        (list phpunit-testable-list)
        (matched nil)
        (match))
    (while (and list (not matched))
      (setq matched (string-match (caar list) file))
      (setq match (car list))
      (setq list (cdr list)))
    (and matched match)))

;;;###autoload
(defun phpunit-test-class-name (&optional file)
  (or (bound-and-true-p phpunit-test-class-name)
      (let* ((file (or file (buffer-file-name)))
             (match (phpunit-get-match file)))
        (and match (or (and (functionp (cadr match))
                            (funcall (cadr match) file))
                       match)))))

;;;###autoload
(defun phpunit-test-file-name (&optional file)
  "Return the class name of the PHPUnit test for `file'"
  (or (bound-and-true-p phpunit-test-file-name)
      (let* ((file (or file (buffer-file-name)))
             (match (phpunit-get-match file)))
        (and match (or (and (functionp (cadr match))
                            (funcall (caddr match) file)) match)))))

;;;###autoload
(defun phpunit-find-test-file ()
  "Open currently visited file's corresponding test file."
  (interactive)
  (let ((file (phpunit-test-file-name))
        (buf))
    (or (and (setq buf (get-file-buffer file)) (pop-to-buffer buf) nil)
        (find-file file))))

(add-hook 'php-mode-hook
          '(lambda ()
             (and (or (phpunit-testablep)
                      (and (buffer-file-name)
                           (string-match "/test\\(s\\)?/" (buffer-file-name)))
                      (string= (substring (buffer-file-name) 0
                                          (length phpunit-run-directory))
                               phpunit-run-directory))
                  (phpunit-minor-mode t))))

(provide 'phpunit)
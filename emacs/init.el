;;; init.el --- GNU Emacs Configuration

;; Copyright (C) 2021 Thomas Tych

;; Author: Thomas Tych <thomas.tych@gmail.com>
;; Created: Sunday April 11, 2021

;; This program is free software. You can redistribute it and/or modify it under
;; the terms of the Do What The Fuck You Want To Public License, version 2 as
;; published by Sam Hocevar.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.
;;
;; You should have received a copy of the Do What The Fuck You Want To Public
;; License along with this program. If not, see http://www.wtfpl.net/.

;;; Commentary:

;; Following lines build the configuration code out of the config.el file.

;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.
(setq gc-cons-threshold (* 50 1000 1000))

(if (file-exists-p (expand-file-name "myinit.el" user-emacs-directory))
    (load-file (expand-file-name "myinit.el" user-emacs-directory))
  (org-babel-load-file (expand-file-name "myinit.org" user-emacs-directory)))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 10 1000 1000))

;;; init.el ends here

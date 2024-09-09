;;; init.el --- GNU Emacs Configuration

;; Copyright (C) 2022 Thomas Tych

;; Author: Thomas Tych <thomas.tych@gmail.com>
;; Created: Saturday February 12, 2022

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

(defvar conf-el (expand-file-name "conf.el" user-emacs-directory))
(defvar conf-org (expand-file-name "conf.org" user-emacs-directory))
(if (and (file-exists-p conf-el)
         (time-less-p (nth 5 (file-attributes conf-org))
                      (nth 5 (file-attributes conf-el))))
    (load-file conf-el)
  (org-babel-load-file conf-org))

;;; init.el ends here

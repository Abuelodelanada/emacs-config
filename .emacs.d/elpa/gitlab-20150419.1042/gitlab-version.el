;;; gitlab-version.el --- Gitlab Emacs client version

;; Copyright (C) 2014 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'pkg-info)


(defun gitlab--library-version ()
  "Get the version in the emacs-gitlab client header."
  (-when-let (version (pkg-info-library-version 'gitlab))
    (pkg-info-format-version version)))

;; (defun gitlab--package-version ()
;;   "Get the package version of emacs-gitlab.
;; This is the version number of the installed emacs-gitlab package."
;;   (-when-let (version (pkg-info-package-version 'emacs-gitlab-mode))
;;     (pkg-info-format-version version)))


(provide 'gitlab-version)
;;; gitlab-version.el ends here

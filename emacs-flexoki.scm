;;; Copyright © 2023 Andrew Jose <arnav.jose@gmail.com>
;;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Andrew Jose <arnav.jose@gmail.com>
;; Maintainer: Andrew Jose <arnav.jose@gmail.com>
;; URL: https://github.com/crmsnbleyd/flexoki-emacs-theme
;; This file is NOT part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (emacs-flexoki)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages wget)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public emacs-flexoki-theme
  (let ((commit "22bace7387e9012002a6a444922f75f9913077b0")
        (revision "1"))
    (package
      (name "emacs-flexoki-theme")
      (version (git-version "0.07" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/crmsnbleyd/flexoki-emacs-theme")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1cq73bdv3lkn8v3nx6aznygqaac9s5i7pvirl8wz9ib31hsgwpbk"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/crmsnbleyd/flexoki-emacs-theme")
      (synopsis "Emacs theme based on Flexoki")
      (description
       "This package contains dark and light versions of the Flexoki colour scheme by Steph Ango")
      (license license:gpl3+))))

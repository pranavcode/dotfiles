;;; colorsarenice-dark-theme.el --- The dark colorsarenice theme. -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/colorsarenice-theme

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2013-2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(eval-when-compile (require 'colorsarenice-common))

(deftheme colorsarenice-dark "The dark colorsarenice theme.")

(colorsarenice--set-faces
 colorsarenice-dark
 ((foreground "#dadada")
  (background "#24231f")
  (region "#4f4f4f")
  (helmselection "#444444")
  (hlline "#333333")
  (highlight "#224422")
  (orange "#edb082")
  (gray "#888888")
  (yellow1 "#d4d484")
  (yellow2 "#c4c490")
  (blue1 "#9f9fdf")
  (blue2 "#88aabb")
  (red1 "#e67c7c")
  (red2 "#cf9d9d")
  (green1 "#a0cca0")
  (green2 "#b3cc8b")
  (purple "#c08ad6")
  (modelinefg "#da9fbf")
  (modelinebg "#3f3d38")
  (fringebg "#181818")
  (whitespaceline "#64231f")
  (whitespacetrailing "#94332f")
  (delim1fg red1)
  (delim2fg red2)
  (delim3fg orange)
  (delim4fg yellow1)
  (delim5fg green1)
  (delim6fg green2)
  (delim7fg blue1)
  (delim8fg blue2)
  (delim9fg purple)
  (unmatcheddelimbg "#aa6666")
  (block1bg "#34231f")
  (block2bg "#24331f")
  (block3bg "#24232f")
  (block4bg "#34331f")
  (block5bg "#24332f")
  (block6bg "#34232f")
  (block7bg "#44332f")
  (block8bg "#34432f")
  (block9bg "#34333f")
  (identifierlightness 70)
  (identifierssaturation 20)))

(provide-theme 'colorsarenice-dark)
;;; colorsarenice-dark-theme.el ends here

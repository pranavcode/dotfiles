;;; colorsarenice-light-theme.el --- The light colorsarenice theme. -*- lexical-binding: t -*-

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

(deftheme colorsarenice-light "The light colorsarenice theme.")

(colorsarenice--set-faces
 colorsarenice-light
 ((foreground "#161616")
  (background "#faf9f0")
  (region "#aaaaaa")
  (helmselection "#cccccc")
  (hlline "#dddddd")
  (highlight "#aaccaa")
  (orange "#a96c2f")
  (gray "#888888")
  (yellow1 "#70702c")
  (yellow2 "#68683c")
  (blue1 "#28286a")
  (blue2 "#1d6a6a")
  (red1 "#b4212c")
  (red2 "#802320")
  (green1 "#1c724c")
  (green2 "#3c721c")
  (purple "#751472")
  (modelinefg "#500040")
  (modelinebg "#dfddd8")
  (fringebg "#dfddd8")
  (whitespaceline "#fac9c0")
  (whitespacetrailing "#fa8980")
  (delim1fg red2)
  (delim2fg red1)
  (delim3fg orange)
  (delim4fg yellow1)
  (delim5fg green1)
  (delim6fg green2)
  (delim7fg blue1)
  (delim8fg blue2)
  (delim9fg purple)
  (unmatcheddelimbg "#dd6666")
  (block1bg "#eaf9f0")
  (block2bg "#fae9f0")
  (block3bg "#faf9e0")
  (block4bg "#eae9f0")
  (block5bg "#fae9e0")
  (block6bg "#eaf9e0")
  (block7bg "#dae9e0")
  (block8bg "#ead9e0")
  (block9bg "#eae9d0")
  (identifierlightness 30)
  (identifierssaturation 35)))

(provide-theme 'colorsarenice-light)
;;; colorsarenice-light-theme.el ends here

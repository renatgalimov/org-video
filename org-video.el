;;; org-video.el --- Export org-mode nodes as video files -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Filename: org-video.el
;; Description: Make videos from org-mode documents
;; Author: Renat Galimov
;; Maintainer:
;; Created: Mon Feb 14 05:12:23 2022 (+0300)
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.2") (org "9.4") (mimetypes "20201115.1605"))
;; Last-Updated: Sun Feb 20 17:29:15 2022 (+0300)
;;           By: Renat Galimov
;;     Update #: 774
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ox-latex)
(require 'org-attach)
(require 'mimetypes)

(defgroup org-video nil
  "Convert org documents to video slideshows."
  :tag "Org Video"
  :group 'org)

(defcustom org-video-default-output-format "mp4"
  "The format of the resulting output file.

To see all output formats supported by ffmpeg run

  ffmpeg -formats"
  :group 'org-video
  :package-version '(org-video . "0.0.1")
  :type '(string :tag "Default output format"))

(defun org-video-export-slide-to-pdf()
  "Export current node as a PDF file."
  (let ((outfile (make-temp-file "org-export" nil ".tex"))
        (org-latex-classes
         '(("slides" "\\documentclass{slides}"
            ("\\huge{%s}\\normalsize" . "\\huge*{%s}\\normalsize")
            ("\\large{%s}\\normalsize" . "\\large*{%s}\\normalsize")))))
    (org-export-to-file
        'latex outfile nil t nil nil
        `(:latex-class "slides"
                       :latex-class-options "[12pt]"
                       :latex-title-command "\\huge{%t}\\normalsize"
                       :latex-toc-command "\n"
                       :latex-header ,(mapconcat
                                       'identity
                                       '("\\usepackage[paperheight=9in, paperwidth=16in]{geometry}"
                                         "\\usepackage{enumitem}"
                                         "\\setlist{noitemsep}") "\n"))
        (lambda (file)
          (let* ((pdf-path (org-latex-compile file nil))
                 (tex-path (concat (file-name-sans-extension pdf-path) ".tex")))
            (delete-file tex-path nil)
            pdf-path)))))

(defun org-video-get-org-link(video-source)
  "Return a path to VIDEO-SOURCE org link."

  (when (not (stringp video-source))
    (error "Argument video-source should be of a string type"))

  (let* ((link-element (with-temp-buffer
	                     (let ((org-inhibit-startup nil))
	                       (insert video-source)
	                       (goto-char (point-min))
	                       (org-element-link-parser))))
         (raw-link (org-element-property :raw-link link-element))
         (path (org-element-property :path link-element)))
    (if (string-prefix-p "attachment:" raw-link)
        (org-attach-expand path)
      path)))


(defun org-video-get-source-path(video-source)
  "Return a source path of the property.

Automatically determine the path if the VIDEO-SOURCE is an org
  link.

Ensure that the video source exists.
Follow symlinks."
  (let ((absolute-path (file-truename (if (string-match-p "\\[\\[.*\\]\\]" video-source)
                                          (org-video-get-org-link video-source)
                                        video-source))))
    (when (file-exists-p absolute-path)
      absolute-path)))

(defun org-video-get-slide-type()
  "Return slide type of the org entry at point.

Possible return values are:
    - latex
    - video

Returns nil if return slide type cannot be determined."
  (let ((video-source (org-entry-get nil "VIDEO-SOURCE")))
    (when (stringp video-source)
      (if (equal "latex" video-source) "latex"
        (let ((file-path (org-video-get-source-path video-source)))
          (if (stringp file-path)
              (let ((mimetype (mimetypes-guess-mime file-path)))
                (when (and (stringp mimetype) (string-match-p "^video/.*$" mimetype))
                  "video"))))))))

(defun org-video-get-source ()
  "Make or get a source link to the slide file."
  (let ((slide-type (org-video-get-slide-type)))
    (cond ((equal slide-type "latex") (org-video-latex-get-source))
          ((equal slide-type "video") (org-video-video-get-source))
          (t nil))))

(defun org-video-latex-get-slide-duration-seconds ()
  "Get how much time the latex SLIDE will be visible on the screen."
  5.0)

(defun org-video-video-get-slide-duration-seconds(slide-source)
  "Get how much time the video slide at SLIDE-SOURCE will be visible on the screen."
  (let ((probe-data (org-video-video-probe slide-source)))
    (string-to-number (gethash "duration" (plist-get probe-data :video-stream)))))

(defun org-video-get-slide-duration-seconds (slide-type slide-source)
  "Get how much time the slide of SLIDE-TYPE at SLIDE-SOURCE will be visible on the screen."
  (cond ((equal slide-type "latex") (org-video-latex-get-slide-duration-seconds))
        ((equal slide-type "video") (org-video-video-get-slide-duration-seconds slide-source))
        (t nil)))

(defun org-video-get-slide()
  "Make a slide object from the org entry at point."
  (let ((slide-type (org-video-get-slide-type)))
    (when (stringp slide-type)
      (let ((slide-source (org-video-get-source)))
      `(:slide-type ,slide-type :slide-source ,(org-video-get-source) :duration ,(org-video-get-slide-duration-seconds slide-type slide-source))))))

(defun org-video-get-slides(&optional scope)
  "Get a list of slide objects in this buffer.

SCOPE determines the scope of this command. Check the SCOPE
documentation of `org-map-entries'."

  (seq-filter (lambda (item) (not (null item))) (org-map-entries (lambda() (org-video-get-slide)) t (or scope 'file))))

(defun org-video-get-filtergraph ()
  (org-video-slides-to-filtergraph (org-video-get-slides)))

(defun org-video-latex-get-source()
  "Get source file for a latex slide."
  (let* ((pdf-path (org-video-export-slide-to-pdf))
         (png-path (org-video-pdf-to-png pdf-path)))
    (delete-file pdf-path nil)
    png-path))

(defun org-video-video-get-source()
  "Get source file for a video slide."
  (let ((video-source (org-entry-get nil "VIDEO-SOURCE")))
    (when (stringp video-source)
      (org-video-get-source-path video-source))))

(defun org-video-file-get-source-path()
  "Get an absolute path to slide's source.

This function checks that the source file exists and raises
errors if it cannot be found."
  (when (not (stringp (org-attach-dir)))
    (error "Org attach directory not set"))

  (let ((source (org-entry-get nil "SOURCE")))
    (when (or (not (stringp source)) (string-blank-p source))
      (error "SOURCE property not defined"))
    (let ((source-path (expand-file-name source (org-attach-dir))))
      (when (not (file-exists-p source-path))
        (error "Source not found: %s/%s" (org-attach-dir) source))
      source-path)))

(defun org-video-pdf-to-png(pdf-path)
  "Convert file at PDF-PATH to a PNG file."
  (let* ((png-path (concat (file-name-sans-extension pdf-path) ".png"))
         (exit-code (call-process
                     "convert" nil "*Convert output*" t
                     "-alpha" "remove" pdf-path "-resize" "1920x1080" "-density" "96" png-path)))

    (if (eq exit-code 0)
        png-path
      (display-buffer "*Convert output*")
      (error "Convert command failed"))))

(defun org-video-slide-filter(input-id duration)
  "Make an ffmpeg filter that plays a slide with INPUT-ID.

DURATION is to control when to fade out."
  (let* ((fade-duration (if (> duration 2) 0.5 nil)))
    (if fade-duration
        (format "[%s]fps=25, scale=w=1920:h=1080:force_original_aspect_ratio=decrease, pad=1920:1080:(ow-iw)/2:(oh-ih)/2, fade=type=in:duration=%s, fade=type=out:start_time=%s:duration=%s[v%s]" input-id fade-duration (- duration fade-duration) fade-duration input-id)
      (format "[%s]fps=25[v%s]" input-id input-id))))

(defun org-video-filter-concat (input-count)
  "Make an ffmpeg filter that concatinates INPUT-COUNT slides into one video slide."
  (format "%s concat=n=%s:v=1:a=0, fps=25, realtime [v]" (mapconcat (lambda (input-id) (format "[v%s]" input-id)) (number-sequence 0 (- input-count 1)) " ") input-count))

(defun org-video-slides-to-filtergraph(slides)
  "Produce an ffmpeg filterfraph for given SLIDES."
  (let* ((input-ids (number-sequence 0 (- (length slides) 1)))
         (slide-filters (cl-mapcar (lambda (slide input-id) (org-video-slide-filter input-id (plist-get slide :duration))) slides input-ids)))
    (mapconcat 'identity (append slide-filters `(,(org-video-filter-concat (length slides)))) "; ")))

(defun org-video-latex-slide-to-ffmpeg-input(slide)
  "Produce ffmpeg input arguments for latex SLIDE."
  `("-r" "1/5" "-i" ,(plist-get slide :slide-source)))

(defun org-video-video-slide-to-ffmpeg-input(slide)
  "Produce ffmpeg input arguments for latex SLIDE."
  `("-i" ,(plist-get slide :slide-source)))

(defun org-video-slide-to-ffmpeg-input(slide)
  "Produce ffmpeg input argument for SLIDE."
  (let ((slide-type (plist-get slide :slide-type)))
    (cond ((equal slide-type "latex") (org-video-latex-slide-to-ffmpeg-input slide))
          ((equal slide-type "video") (org-video-video-slide-to-ffmpeg-input slide))
          (t (error "Unsupported slide type: %s" slide-type)))))

(defun org-video-slides-to-ffmpeg-inputs(slides)
  "Produce ffmpeg input arguments for SLIDES."
  (apply 'cl-concatenate 'list
         (mapcar #'org-video-slide-to-ffmpeg-input slides)))

(defun org-video-slides-to-ffmpeg-args(slides)
  "Produce ffmpeg arguments to play SLIDES."
  (cl-concatenate
   'list
   (org-video-slides-to-ffmpeg-inputs slides)
   `("-filter_complex" ,(org-video-slides-to-filtergraph slides))
   ))

(defun org-video-slides-to-dot()
  "Produce a dot file of a filegraph for given SLIDES."
  (let* ((slides (org-video-get-slides))
         (input-ids (number-sequence 0 (- (length slides) 1)))
         (inputs (mapconcat (lambda (input-id) (format "nullsrc [%s]" input-id)) input-ids "; "))
         (filtergraph (org-video-slides-to-filtergraph slides))
         (full-graph (concat inputs ";" filtergraph "; [v] nullsink"))
         (full-graph-file (make-temp-file "graph2dot" nil ".txt" full-graph))
         (out-dot-file (make-temp-file "graph2dot" nil ".dot"))
         (out-png-file (make-temp-file "graph2dot" nil ".png")))
    (call-process
     "graph2dot" full-graph-file "*graph2dot output*" t "-o" out-dot-file)
    (delete-file full-graph-file)
    (call-process
     "dot" out-dot-file "*dot output" t "-Tpng" "-o" out-png-file)
    (delete-file out-dot-file)
    (find-file-other-window out-png-file)))

(defun org-video-video-stream-p(stream)
  "Check whether the STREAM document is a video stream."
  (when (equal (gethash "codec_type" stream) "video") stream))

(defun org-video-get-first-video-stream (streams)
  "Get the first video stream from the list of STREAMS."
  (seq-some #'org-video-video-stream-p streams))

(defun org-video-video-probe(video-path)
  "Use ffprobe to detect video parameters of a file at VIDEO-PATH."
  (with-temp-buffer
    (let* ((stderr-file (make-temp-file "ffprobe")))
      (unwind-protect
          (let ((exit-code
                 (call-process "ffprobe" nil `(t ,stderr-file) nil "-print_format" "json" "-show_streams" video-path)))
            (when (not (= exit-code 0))
              (find-file-other-window stderr-file)
              (error "Cannot probe the video")))
        (delete-file stderr-file)))
    (goto-char (point-min))
    (let* ((video-info (json-parse-buffer))
           (streams (gethash "streams" video-info)))
      `(:video-stream ,(org-video-get-first-video-stream streams)))))

(defun org-video-preview-command()
  (let ((slides (org-video-get-slides)))
    (mapconcat 'shell-quote-argument
               (apply 'cl-concatenate 'list `(("ffmpeg") ,(org-video-slides-to-ffmpeg-args slides) ("-map" "[v]" "-r" "25" "-f" "opengl" "org-ffmpeg"))) " ")))

(defun org-video-preview()
  (async-shell-command (org-video-preview-command)))

;;; Interactive function


;;;###autoload
(defun org-video-export-to-video
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a video file.

ASYNC is ignored.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

BODY-ONLY is ignored."

  (let ((outfile (org-export-output-file-name ".mp4" subtreep)))

    ))


(provide 'org-video)
;;; org-video.el ends here

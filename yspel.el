;;; yspel.el --- 

;; Copyright (C) 2010  yama

;; Author: yama <yama@yama-desktop>
;; Keywords: 
;; 	$Id: yspel.el,v 1.22 2010/09/11 06:06:05 yama Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Yahoo! Japan の校正支援サービスを利用した校正支援ツールです。
;;

;;; Usage

;; (require 'yspel)
;; M-x yspel

;;; 制限
;; Yahoo の制限により、一日に利用回数は50000件、ファイルサイズは100KBまでです。
;; 4000文字くらいが現実的？

;;; Code:

(require 'url)
(require 'xml)

;;; configuration

 
(defvar yspel-check-hyouki 1
  "1: 表記・表現の間違いや不適切な表現に関する指摘
　 －誤変換、誤用、使用注意語、不快語（使用不適切な語や隠語など）、
　 機種依存文字または拡張文字、外国地名、固有名詞、人名、ら抜き言
　 葉 が指摘されます。")

(defvar yspel-check-youji 2
  "2: わかりやすい表記にするための指摘
　 －当て字、表外漢字、用字（※） が指摘されます。
　 ※日本新聞協会「新聞用語集」、共同通信社「記者ハンドブック」を
　 主な参考としています。")

(defvar yspel-check-yougo 3
  "3: 文章をよりよくするための指摘
　 －用語言い換え、二重否定、助詞不足の可能性あり、冗長表現、
　 略語 が指摘されます。")


;; 	filter_groupで指定した指摘グループから除外する指摘を指定します。
;; グループ1
(defvar yspel-filter-gohenkan nil
  "1: 誤変換　　例：人事異同→人事異動")
(defvar yspel-filter-goyou nil
  "2: 誤用　　例：煙に巻く→けむに巻く")
(defvar yspel-filter-shiyoutyuui nil
  "3: 使用注意　　例：外人墓地→外国人墓地")
(defvar yspel-filter-hukaigo nil
  "4: 不快語　　例：がんをつける→にらむ")
(defvar yspel-filter-kisyuizon nil
  "5: 機種依存または拡張文字（EUC表示不可の場合も指摘されます）　例：○付き数字、一部の旧字体など")
(defvar yspel-filter-gaikokutimei nil
  "6: 外国地名　　例：モルジブ→モルディブ")
(defvar yspel-filter-koyuumeisi nil
  "7: 固有名詞　　例：ヤフーブログ→Yahoo!ブログ")
(defvar yspel-filter-jinnmei nil
  "8: 人名　　例：ベートーヴェン→ベートーベン")
(defvar yspel-filter-ranuki nil
  "9: ら抜き　　例：食べれる→食べられる")

;; グループ2
(defvar yspel-filter-ateji nil
  "10: 当て字　　例：出鱈目、振り仮名")
(defvar yspel-filter-hyougaikanji nil
  "11: 表外漢字あり　　例：灯籠→灯●")
(defvar yspel-filter-youji nil
  "12: 用字　　例：曖昧→あいまい")

;; グループ3
(defvar yspel-filter-iikae nil
  "13: 用語言い換え（商標など）　　例：セロテープ→セロハンテープ")
(defvar yspel-filter-nijyuhitei nil
  "14: 二重否定　　例：聞かなくはない")
(defvar yspel-filter-jyoshi nil
  "15: 助詞不足の可能性あり　　例：学校行く")
(defvar yspel-filter-jyoutyou nil
  "16: 冗長表現　　例：ことができます")
(defvar yspel-filter-ryakugo nil
  "17: 略語　　例：ADSL→非対称デジタル加入者線(ADSL)")

(defconst yspel-id "YaNTsVexg64rs30SK43AjYvy3NWHMbT4BdjCOn0Sav7z_pQuShdo8bhgmdJMN159UAM-")

(defvar yspel-coding-system 'utf-8)

(defconst yspel-buffer-name "*yspel*"
  "yspelバッファの名前")

(defvar yspel-target-buffer nil)

(defvar yspel-start-point 0
  "start point.")

(defvar yspel-mode nil
  "Non-nil if Yspel mode is enabled.
Don't change this variable directly, you must change it by one of the
functions that enable or disable Yspel Keyword mode.")

;;  Define keymap
(defvar yspel-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map " " 'scroll-up)
	(define-key map "b" 'scroll-down)
	(define-key map "n" 'forward-line)
	(define-key map "p" 'yspel-previous-line)
	(define-key map "q" 'delete-window)
	(define-key map "\C-m" 'yspel-keyword-jump)
	map))
;;--------------------------------------------------------------------------

(defun yspel-previous-line ()
  "previous-line."
  (interactive)
  (forward-line -1))

(defun yspel-check-group ()
   "make check group list."
   (if yspel-check-hyouki (setq yspel-check-hyouki 1))
   (if yspel-check-youji  (setq yspel-check-youji  2))
   (if yspel-check-yougo  (setq yspel-check-yougo  3))
   (mapconcat 'number-to-string (delq nil (list
										   yspel-check-hyouki
										   yspel-check-youji
										   yspel-check-yougo)) ","))
   
(defun yspel-filter-group ()
   "make filter group list."
   (if yspel-filter-gohenkan 	  (setq yspel-filter-gohenkan      1))
   (if yspel-filter-goyou  	      (setq yspel-filter-goyou         2))
   (if yspel-filter-shiyoutyuui   (setq yspel-filter-shiyoutyuui   3))
   (if yspel-filter-hukaigo 	  (setq yspel-filter-hukaigo       4))
   (if yspel-filter-kisyuizon     (setq yspel-filter-kisyuizon     5))
   (if yspel-filter-gaikokutimei  (setq yspel-filter-gaikokutimei  6))
   (if yspel-filter-koyuumeisi    (setq yspel-filter-koyuumeisi    7))
   (if yspel-filter-jinnmei 	  (setq yspel-filter-jinnmei       8))
   (if yspel-filter-ranuki 	      (setq yspel-filter-ranuki        9))
   (if yspel-filter-ateji  	      (setq yspel-filter-ateji         10))
   (if yspel-filter-hyougaikanji  (setq yspel-filter-hyougaikanji  11))
   (if yspel-filter-youji  	      (setq yspel-filter-youji         12))
   (if yspel-filter-iikae  	      (setq yspel-filter-iikae         13))
   (if yspel-filter-nijyuhitei    (setq yspel-filter-nijyuhitei    14))
   (if yspel-filter-jyoshi 	      (setq yspel-filter-jyoshi        15))
   (if yspel-filter-jyoutyou      (setq yspel-filter-jyoutyou      16))
   (if yspel-filter-ryakugo       (setq yspel-filter-ryakugo       17))
   (mapconcat 'number-to-string (delq nil (list
										   yspel-filter-gohenkan
										   yspel-filter-goyou
										   yspel-filter-shiyoutyuui
										   yspel-filter-hukaigo
										   yspel-filter-kisyuizon
										   yspel-filter-gaikokutimei
										   yspel-filter-koyuumeisi
										   yspel-filter-jinnmei
										   yspel-filter-ranuki
										   yspel-filter-ateji
										   yspel-filter-hyougaikanji
										   yspel-filter-youji
										   yspel-filter-iikae
										   yspel-filter-nijyuhitei
										   yspel-filter-jyoshi
										   yspel-filter-jyoutyou
										   yspel-filter-ryakugo)) ","))


(defun yspel-url-encode (strings &optional coding-system)
  (mapconcat
   (lambda (c)
     (format (if (string-match "[a-zA-Z0-9]" (char-to-string c))
		 "%c" "%%%02X") c))
   (encode-coding-string strings (or coding-system yspel-coding-system)) ""))

(defun yspel-data-restructure (strings)
  "request data."
  (concat "appid=" yspel-id
 		  "&sentence=" (yspel-url-encode strings yspel-coding-system)
 		  "&filter_group=" (yspel-check-group)
 		  "&no_filter=" (yspel-filter-group)))

(defun yspel-get (strings)
  (interactive)
  (let ((url-request-method "POST")
		(url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
		(url-request-data (yspel-data-restructure strings)))
	(set-buffer (url-retrieve-synchronously "http://jlp.yahooapis.jp/KouseiService/V1/kousei")))
  (if (string-match "200 OK" (buffer-substring (point-min) 20))
	  (progn
		(let* ((root (car (xml-parse-region (point-min) (point-max))))
			   (Result (xml-get-children root 'Result)) ; Resultを格納。
			   (node-count (length Result))             ; 個数。
			   (num 0))
		  (if  (= 0 node-count) (message "%s" "校正箇所はありませんでした")
			(if (yspel-buffer-p) (setq buffer-read-only nil))
			(unless (yspel-buffer-p) (set-buffer (get-buffer-create yspel-buffer-name)))
			(erase-buffer)
			(while (< num node-count)
			  (let ((Line (car (xml-node-children
								(car (xml-get-children (nth num Result) 'StartPos)))))
					(Length (car (xml-node-children
								  (car (xml-get-children (nth num Result) 'Length)))))
					(Surface (car (xml-node-children
								   (car (xml-get-children (nth num Result) 'Surface)))))
					(ShitekiWord (car (xml-node-children
									   (car (xml-get-children (nth num Result) 'ShitekiWord)))))
					(ShitekiInfo (car (xml-node-children
									   (car (xml-get-children (nth num Result) 'ShitekiInfo))))))
				(setq num (+ 1 num))
				(if (/= yspel-start-point 0)
					(setq Line (number-to-string (+ yspel-start-point (string-to-number Line)))))
				(yspel-format Line Length Surface ShitekiWord ShitekiInfo)))
			(yspel-window-set))))
  	  (message "%s" "Error")))

(defun yspel-window-set ()
  "SET yspel window."
  (goto-char (point-min))
  (if (yspel-window-p)
	  (progn
		(select-window (yspel-window-p))
		(yspel-mode))
	(split-window)
	(set-window-buffer (next-window) (current-buffer))
	(select-window (next-window))
	(yspel-mode)))

(defun yspel-format (Line Length Surface ShitekiWord ShitekiInfo)
  "Format yspel window."
  (let ((lin (format "%6d:" (string-to-number Line)))
		(len (format "%d   " (string-to-number Length)))
		(word (if (not Surface) nil
				(format "%-12s" (decode-coding-string Surface yspel-coding-system))))
		(keyword (if (not ShitekiWord) "\t\t\t"
				   (format "%-12s" (concat "[" (decode-coding-string
								ShitekiWord yspel-coding-system) "]"))))
		(sinfo (if (not ShitekiInfo) nil
				 (decode-coding-string ShitekiInfo yspel-coding-system))))
	(insert (concat lin len "\t" word "\t" keyword "\t<" sinfo ">\n"))))

(defun yspel-keyword-jump ()
  "jumo to point."
  (interactive)
  (switch-to-buffer-other-window yspel-target-buffer))

(defun yspel-region-active-p ()
  "region active t."
  (if (and transient-mark-mode mark-active) t nil))

(defun yspel ()
  "Yahoo API による日本語校正支援システム。"
  (interactive )
  (set-buffer (current-buffer))
  (setq yspel-target-buffer (buffer-name))
  (if (yspel-region-active-p)
	  (progn
		(setq yspel-start-point (region-beginning))
		(yspel-get (buffer-substring (region-beginning) (region-end))))
	(setq yspel-start-point 0)
	(yspel-get (buffer-string))))

(defun yspel-buffer-p ()
  "yspelバッファの存在チェック"
  (if (get-buffer yspel-buffer-name)
	  (set-buffer yspel-buffer-name)))

(defun yspel-window-p ()
  "yspelウィンドウのチェック。"
  (get-buffer-window (get-buffer yspel-buffer-name)))

(defun yspel-mode ()
  "yspel occur mode.
query to yahoo API
\\<yspel-mode-map>

key     binding
---     -------
\\[delete-window]		quit
\\[scroll-up]		scroll-up
\\[scroll-down]		scroll-down
\\[forward-line]		next-line
\\[yspel-previous-line]		previous-line
\\[yspel-keyword-jump]		jump-to-keyword
"
  (interactive)
  (setq major-mode 'yspel-mode
		mode-name "yspel"
		buffer-read-only t)
  (use-local-map yspel-mode-map)
  (make-local-hook 'post-command-hook)
  (make-local-hook 'pre-command-hook)
  (setq post-command-hook 'yspel-post-command-hook)
  (setq pre-command-hook 'yspel-pre-command-hook)
  (run-hooks 'yspel-mode-hook))

(defun yspel-post-command-hook ()
    "line number echo."
    (interactive)
	(beginning-of-line)
	(if (= (point) (point-max)) nil
	  (let ((var1 (string-to-number (buffer-substring (point) (+ (point) 6))))
			(len (string-to-number (buffer-substring (+ (point) 7) (+ (point) 10)))))
		(switch-to-buffer-other-window  yspel-target-buffer)
		(if (yspel-region-active-p) (deactivate-mark))
		(yspel-highlight 0 (+ 1 var1) (+ var1 (+ 1 len)))
		(goto-char var1)
		(switch-to-buffer-other-window  yspel-buffer-name)
		(save-excursion
		  (yspel-highlight 1 (progn (beginning-of-line) (point))
						   (progn (end-of-line) (point)))))))

(defun yspel-pre-command-hook ()
   "Remove highlighing in both source and output buffers."
   ;; used as pre command hook in *toc* buffer
  		 (yspel-unhighlight 0)
  		 (yspel-unhighlight 1))

;;; オーバーレイ
;; We keep a vector with several different overlays to do our highlighting.
(defvar yspel-highlight-overlays [nil nil])

;; Initialize the overlays
(aset yspel-highlight-overlays 0 (make-overlay 1 1))
(overlay-put (aref yspel-highlight-overlays 0) 'face 'highlight)
(aset yspel-highlight-overlays 1 (make-overlay 1 1))
(overlay-put (aref yspel-highlight-overlays 1) 'face 'highlight)

;; Two functions for activating and deactivation highlight overlays
(defun yspel-highlight (index begin end &optional buffer)
  "Highlight a region with overlay INDEX."
  (move-overlay (aref yspel-highlight-overlays index)
                begin end (or buffer (current-buffer))))

(defun yspel-unhighlight (index)
  "Detatch overlay INDEX."
  (delete-overlay (aref yspel-highlight-overlays index)))



(provide 'yspel)
;;; yspel.el ends here

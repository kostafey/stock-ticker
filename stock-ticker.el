;;; stock-ticker.el --- Show stock prices in mode line

;; Copyright (C) 2015 Gunther Hagleitner

;; Author: Gunther Hagleitner
;; Version: 0.1
;; Keywords: comms
;; URL: https://github.com/hagleitn/stock-ticker
;; Package-Requires: ((s "1.9.0") (request "0.2.0") (drawille "0.1"))

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
;;
;; Stock-ticker lets you display stock prices in the mode line. You
;; can specify a number of stock symbols to track and their current
;; price, change and change percentage will be rotated in the mode
;; line.
;;
;; The financial data will be retrieved from Yahoo's finance apis via
;; YQL.

;;; Code:
(require 'json)
(require 'request)
(require 's)
(require 'timer)
(require 'drawille)

(defface stock-ticker--grow-face
  '((t :foreground "#119911"))
  "Face for the `header-line'."
  :group 'stock-ticker-faces)

(defface stock-ticker--reduce-face
  '((t :foreground "#cc0000"))
  "Face for the `header-line'."
  :group 'stock-ticker-faces)

(defun stock-ticker--query (symbols)
  "Generate yql query string from list of SYMBOLS."
  (let ((query-template "select * from yahoo.finance.quotes where symbol in (\"%s\")")
        (symbol-string (s-join "\",\"" symbols)))
    (format query-template symbol-string)))

(defun stock-ticker--history-query (symbol from-date to-date)
  "Generate yql query string for SYMBOL history with FROM-DATE TO-DATE period."
  (let ((query-template
         (concat "select * from yahoo.finance.historicaldata where symbol in "
                 "('%s') and startDate = '%s' and endDate = '%s'")))
    (format query-template
            symbol
            (cl-multiple-value-bind (month day year) from-date
              (format "%s-%02d-%02d" year month day))
            (cl-multiple-value-bind (month day year) to-date
              (format "%s-%02d-%02d" year month day)))))

(defun stock-ticker--parse (data)
  "Parse financial DATA into list of display strings."
  (let ((qs (assoc-default 'quote (assoc-default 'results (assoc-default 'query data)))))
    (mapcar
     (lambda (q)
       (let ((percent (assoc-default 'PercentChange q))
             (change (assoc-default 'Change q))
             (symbol (assoc-default 'Symbol q))
             (price (assoc-default 'LastTradePriceOnly q))
             (name (assoc-default 'Name q)))
         (format " %s: %s %s (%s) "
                 (if (or
                      (string-match "=" symbol)
                      (string-match "\\^" symbol)) name symbol)
                 (if price price "")
                 (if change change "")
                 (if percent percent ""))))
     qs)))

(defun stock-ticker--color-changes (change)
  (let ((x (string-to-number change)))
    (cond ((> x 0) (propertize change 'face 'stock-ticker--grow-face))
          ((< x 0) (propertize change 'face 'stock-ticker--reduce-face))
          (t change))))

(defun stock-ticker--parse-to-string (data)
  "Parse financial DATA into string."
  (mapconcat
   'identity
   (let ((qs (assoc-default 'quote (assoc-default 'results (assoc-default 'query data)))))
     (mapcar
      (lambda (q)
        (let ((percent (assoc-default 'PercentChange q))
              (change (assoc-default 'Change q))
              (symbol (assoc-default 'Symbol q))
              (price (assoc-default 'LastTradePriceOnly q))
              (name (assoc-default 'Name q))
              (dividend (assoc-default 'DividendYield q)))
          (format "%s%s: %s %s (%s)%s"
                  name
                  (if (or
                       (string-match "=" symbol)
                       (string-match "\\^" symbol)
                       (string-match "NYM" symbol))
                      ""
                    (format " (%s)" symbol))
                  (if price price "")
                  (if change (stock-ticker--color-changes change) "")
                  (if percent percent "")
                  (if dividend (format " dividend: %s%%" dividend) ""))))
      qs)) "\n"))

;;;###autoload
(defgroup stock-ticker nil
  "Stock ticker."
  :group 'applications
  :prefix "stock-ticker-")

;;;###autoload
(defcustom stock-ticker-symbols '("^gspc" "DIA" "^ixic" "^tnx"
                                  "^nya" "XAUUSD=X" "EURUSD=X")
  "List of ticker symbols that the mode line will cycle through."
  :type '(string)
  :group 'stock-ticker)

;;;###autoload
(defcustom stock-ticker-update-interval 300
  "Number of seconds between rest calls to fetch data."
  :type 'integer
  :group 'stock-ticker)

;;;###autoload
(defcustom stock-ticker-display-interval 10
  "Number of seconds between refreshing the mode line."
  :type 'integer
  :group 'stock-ticker)

(defvar stock-ticker--current "")
(defvar stock-ticker--current-stocks nil)
(defvar stock-ticker--current-index 0)
(defvar stock-ticker--update-timer nil)
(defvar stock-ticker--display-timer nil)

(defun stock-ticker--update ()
  "Update the global stock-ticker string."
  (request
   "http://query.yahooapis.com/v1/public/yql"
   :params `((q . ,(stock-ticker--query stock-ticker-symbols))
             (env . "http://datatables.org/alltables.env")
             (format . "json"))
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (when data
                 (progn (setq stock-ticker--current-stocks
                              (stock-ticker--parse data))))))))

(defun stock-ticker--list ()
  "Request all required stock data list and display it to temporary buffer."
  (interactive)
  (request
   "http://query.yahooapis.com/v1/public/yql"
   :params `((q . ,(stock-ticker--query stock-ticker-symbols))
             (env . "http://datatables.org/alltables.env")
             (format . "json"))
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (when data
                 (switch-to-buffer "*stock-ticker*")
                 (erase-buffer)
                 (insert (stock-ticker--parse-to-string data))
                 (message "Updated stock")))))
  (message "Requesting stock..."))

(defun stock-ticker--history (symbol)
  "Request stock data history for SYMBOL."
  (interactive)
  (request
   "http://query.yahooapis.com/v1/public/yql"
   :params `((q . ,(stock-ticker--history-query
                    symbol
                    (cl-multiple-value-bind (_ _ _ day month year)
                        (decode-time (time-subtract (current-time) (days-to-time 365)))
                      (list month day year))
                    (calendar-current-date)))
             (env . "http://datatables.org/alltables.env")
             (format . "json"))
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (when data
                 (switch-to-buffer "*stock-history*")
                 (erase-buffer)
                 (insert (format "%s" data))
                 (message "Updated stock")))))
  (message "Requesting stock history..."))

(defun stock-ticker--next-symbol ()
  "Cycle throug the available ticker symbols and update the mode line."
  (when stock-ticker--current-stocks
    (progn
      (setq stock-ticker--current-index
            (mod (+ stock-ticker--current-index 1)
                 (length stock-ticker--current-stocks)))
      (setq stock-ticker--current
            (nth stock-ticker--current-index stock-ticker--current-stocks))
      (force-mode-line-update))))

;;;###autoload
(define-minor-mode stock-ticker-global-mode
  "Add stock ticker info to the mode line.

Enabeling stock ticker global mode will add stock information in the form
SYMBOL: PRICE CHANGE (PERCENT CHANGE) to the mode line for each stock symbol
listed in 'stock-ticker-symbols'. Only one symbol is displayed at a time and
the mode cycles through the requested symbols at a configurable interval."
  :global t
  :group 'stock-ticker
  (setq stock-ticker--current "")
  (setq stock-ticker--current-index 0)
  (setq stock-ticker--current-stocks nil)
  (when (not global-mode-string) (setq global-mode-string '("")))
  (when stock-ticker--update-timer (cancel-timer stock-ticker--update-timer))
  (when stock-ticker--display-timer (cancel-timer stock-ticker--display-timer))
  (if (not stock-ticker-global-mode)
      (setq global-mode-string
            (delq 'stock-ticker--current global-mode-string))
    (add-to-list 'global-mode-string 'stock-ticker--current t)
    (setq stock-ticker--update-timer
          (run-at-time nil stock-ticker-update-interval
                       'stock-ticker--update))
    (setq stock-ticker--display-timer
          (run-at-time nil stock-ticker-display-interval
                       'stock-ticker--next-symbol))
    (stock-ticker--update)))

(defun stock-ticker--join-dots (prev new)
  (drawille-vector-to-char
   (vconcat (list (if (= prev 3) 1 0) (if (= new 3) 1 0)
                  (if (= prev 2) 1 0) (if (= new 2) 1 0)
                  (if (= prev 1) 1 0) (if (= new 1) 1 0)
                  (if (= prev 0) 1 0) (if (= new 0) 1 0)))))

(defun stock-ticker--draw-chart (prices)
  "Draw chart for normalized PRICES list."
  (let ((mi (apply 'min prices))
        (ma (apply 'max prices))
        (c 0)
        (prev-pos nil)
        (prev-dot nil))
    (dotimes (number (1+ (ceiling (/ (- ma mi) 4.0))) nil)
      (dotimes (number (length prices) nil)
        (insert
         (propertize (drawille-draw-dot nil 0 0)
                     'face
                     `((:foreground ,(face-attribute 'default :background))))))
      (insert "\n"))
    (cl-labels ((draw-dot (price)
                          (let* ((dot (mod (- price mi) 4))
                                 (y (1- (ceiling
                                         (abs
                                          (/ (-
                                              (* (+ (/ ma 4)
                                                    (if (> (mod ma 4) 0) 1 0))
                                                 4)
                                              (- price mi))
                                             4.0)))))
                                 (x (ceiling (ffloor (/ c 2)))))
                            (artist-move-to-xy x y)
                            (delete-char 1)
                            (if (equal (list x y) prev-pos)
                                (insert (stock-ticker--join-dots prev-dot dot))
                              (insert (drawille-draw-dot nil
                                                         (if (oddp c) 1 0)
                                                         dot)))
                            (sit-for 0.01)
                            (setq prev-dot dot)
                            (setq prev-pos (list x y)))
                          (setq c (1+ c))))
      (mapcar
       #'draw-dot
       prices))
    (save-excursion
      (artist-move-to-xy (+ (/ (length prices) 2) 2) 0)
      (insert (number-to-string ma))
      (artist-move-to-xy (+ (/ (length prices) 2) 2)
                         (ceiling (/ (- ma mi) 4.0)))
      (insert (number-to-string mi)))
    (artist-forward-char 2)
    (let ((l (car (last prices))))
      (if (and (not (= l ma))
               (not (= l mi)))
          (progn
            (insert (number-to-string l)))))
    (end-of-buffer)))

(provide 'stock-ticker)
;;; stock-ticker.el ends here

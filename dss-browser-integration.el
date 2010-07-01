(defun view-url ()
  "Open a new buffer containing the contents of URL.
http://github.com/technomancy/emacs-starter-kit/blob/master/starter-kit-defuns.el"
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mozilla integration
; FF settings in about:config
;   extensions.mozrepl.autoStart;true
;   extensions.mozrepl.initUrl;file://localhost/home/tavis/tr_toolchain/mozrepl/custom.js

(require 'moz)
(require 'json)
(defun moz-connect()
  (interactive)
  (global-set-key "\C-x\C-g"
                  (lambda ()
                    (interactive)
                    (save-buffer)
                    (dss/moz-eval-expression "this.BrowserReload()\n"))))

(defun dss/moz-eval-expression (exp)
  "Send expression to Moz."
  (interactive "sJSEval: ")
  (comint-send-string (inferior-moz-process) exp))

(defun moz-reload ()
  "Reload the url in the current tab"
  (interactive)
  (dss/moz-eval-expression "this.BrowserReload()\n"))

(defun moz-find-next ()
  (interactive)
  (dss/moz-eval-expression "gBrowser.webBrowserFind.findNext()\n"))
(defun moz-find-next ()
  (interactive)
  (dss/moz-eval-expression "gBrowser.webBrowserFind.findNext()\n"))
;;;

(defun dss/moz-new-tab ()
  (interactive)
  (dss/moz-eval-expression "gBrowser.selectedTab = gBrowser.addTab()\n"))

(defun dss/moz-duplicate-tab ()
  (interactive)
  (dss/moz-eval-expression "gBrowser.duplicateTab(gBrowser.mCurrentTab)\n"))

(defun dss/moz-close-tab ()
  (interactive)
  (dss/moz-eval-expression "gBrowser.removeCurrentTab()\n"))

(defun dss/moz-back ()
  (interactive)
  (dss/moz-eval-expression "this.BrowserBack()\n"))

(defun dss/moz-forward ()
  (interactive)
  (dss/moz-eval-expression "this.BrowserForward()\n"))

(defun dss/moz-next-tab ()
  (interactive)
  (dss/moz-eval-expression "
var tabbrowser = window.getBrowser();
var _lastTab = tabbrowser.selectedTab;
tabbrowser.selectedTab = tabbrowser.mTabs[_lastTab._tPos+1];
"))

(defun dss/moz-previous-tab ()
  (interactive)
  (dss/moz-eval-expression "
var tabbrowser = window.getBrowser();
var _lastTab = tabbrowser.selectedTab;
tabbrowser.selectedTab = tabbrowser.mTabs[_lastTab._tPos-1];
"))

(defun dss/moz-select-tab (n)
  (interactive "nTab: ")
  (dss/moz-eval-expression
   (format "window.getBrowser().selectedTab = window.getBrowser().mTabs[%d];\n" (- n 1))))

;;; window.getBrowser().mTab[i]
(defun moz-update (&rest ignored)
  "Update the remote mozrepl instance"
  (interactive)
  (comint-send-string (inferior-moz-process)
    (concat "content.document.body.innerHTML="
             (json-encode (buffer-string)) ";")))

(defun moz-enable-auto-reload ()
  "Automatically reload the remote mozrepl when this buffer changes"
  (interactive)
  (add-hook 'after-save-hook 'moz-reload t t))

(defun moz-disable-auto-reload ()
  "Disable automatic mozrepl updates"
  (interactive)
  (remove-hook 'after-save-hook 'moz-reload t))

(add-hook 'js2-mode-hook (lambda (moz-minor-mode 1)))
;    * C-c C-s: open a MozRepl interaction buffer and switch to it
;    * C-c C-l: save the current buffer and load it in MozRepl
;    * C-M-x: send the current function (as recognized by c-mark-function) to MozRepl
;    * C-c C-c: send the current function to MozRepl and switch to the interaction buffer
;    * C-c C-r: send the current region to MozRepl

; http://cpansearch.perl.org/src/ZIGOROU/MozRepl-Plugin-LinkTools-0.01/lib/MozRepl/Plugin/OpenNewTab.pm
; function (url, selected) {
;   selected = (selected) ? true : false;
;   var tab;
;   try {
;     tab = window.top.getBrowser ().addTab (url);
;
;     if (!tab) {
;       return false;
;     }
;
;     if (selected) {
;       window.top.getBrowser ().selectedTab = tab;
;     }
;   }
;   catch (e) {
;     return false;
;   }
;   return true;
; }

; http://cpansearch.perl.org/src/ZIGOROU/MozRepl-Plugin-LinkTools-0.01/lib/MozRepl/Plugin/Location.pm
; function (all) {
;   function getLocationAsJSON (tWindow) {
;     var json = {};
;     for (var p in tWindow.location) {
;       var type;
;
;       try {
;         type = typeof tWindow.location [p];
;       }
;       catch (e) {
;         continue;
;       }
;
;       if (type == "object" || type == "function" || type == "undefined") {
;         continue;
;       }
;       json [p] = tWindow.location [p];
;     }
;     return json;
;   }
;
;   if (all) {
;       return JSONstring.make (Array.prototype.map.call (
;           window.getBrowser ().tabContainer.childNodes,
;           function (tab) {
;               return JSONString.make (getLocationAsJSON (tab.linkedBrowser.contentWindow));
;           }));
;   }
;   else {
;       return JSONstring.make (getLocationAsJSON (window.getBrowser ().contentWindow));
;   }
; }

; http://cpansearch.perl.org/src/ZIGOROU/MozRepl-Plugin-LinkTools-0.01/lib/MozRepl/Plugin/PageReload.pm
; (function (args) {
;   try {
;     if (args.regex && args.regex instanceof RegExp) {
;       var rcnt = 0;
;
;       Array.prototype.forEach.call (
;                                    window.getBrowser ().tabContainer.childNodes,
;                                    function (tab) {
;                                      var tLocation = tab.linkedBrowser.contentWindow.location;
;
;                                      if (args.regex.test (tLocation.href)) {
;                                        tLocation.reload ();
;                                        rcnt++;
;                                      }
;                                    });
;
;       return rcnt;
;     }
;     else if (typeof args.tab_index == "number") {
;       window.getBrowser ().getBrowserAtIndex (tab_index).linkedBrowser.contentWindow.location.reload ();
;       return 1;
;     }
;     else {
;       window.getBrowser ().contentWindow.location.reload ();
;       return 1;
;     }
;   }
;   catch (e) {
;     return 0;
;   }
; }) ({ tab_index: [% tab_index %], regex: [% regex %] });
;

;(require 'webjump)
;(global-set-key [f2] 'webjump)
;(setq webjump-sites
;      (append '(
;  ("Google Image Search" .
;   [simple-query "images.google.com" "images.google.com/images?hl=en&q=" ""])
;  ("Flickr Search" .
;   [simple-query "www.flickr.com" "flickr.com/search/?q=" ""])
;  )
;       webjump-sample-sites))
;

;; http://www.rlazo.org/blog/entry/2008/sep/13/do-you-use-google-to-find-definitions/
(require 'mm-url)
(defun dss/google-define-word-or-phrase (query)
  (interactive "sInsert word or phrase to search: ")
  (let* ((url (concat "http://www.google.com.pe/search?hl=en&q=define%3A"
              (replace-regexp-in-string " " "+" query)))
     (definition
       (save-excursion
         (with-temp-buffer
           (mm-url-insert url)
           (goto-char (point-min))
           (if (search-forward "No definitions found of " nil t)
           "No definitions found"
         (buffer-substring (search-forward "<li>") (- (search-forward "<") 1)))))))
    (message "%s: %s" query definition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-w3m")
(setq w3m-icon-directory "/usr/share/emacs/etc/emacs-w3m")
(require 'w3m-load)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-browser-integration)

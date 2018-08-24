;;; init-org-babel-cider --- Clojure/CIDER configuration for Org-Mode Babel

;;; Commentary:

;; https://github.com/stuartsierra/dotfiles/blob/4ca61e69f89095e5572fcd095ffb3aa896da1f19/.emacs.d/local/org-babel-cider.el

;;; Code:

(require-package 'cider)
(require-package 'ob)
(require-package 'ob-tangle)

(defcustom org-babel-cider-sync-request-timeout 10
  "The number of seconds to wait for a sync response.
  Setting this to nil disables the timeout functionality."
  :type 'integer
  :group 'org-babel-cider)

(defun org-babel-cider-eval-sync-handler (buffer)
  "Make a synchronous request handler for BUFFER that also
  catches errors."
  (nrepl-make-response-handler
   buffer
   (lambda (_buffer value)
     (setq org-babel-cider-sync-response
           (plist-put org-babel-cider-sync-response :value value)))
   (lambda (_buffer out)
     (let ((so-far (plist-get org-babel-cider-sync-response :stdout)))
       (setq org-babel-cider-sync-response
             (plist-put org-babel-cider-sync-response
                        :stdout (concat so-far out)))))
   (lambda (_buffer err)
     (let ((so-far (plist-get org-babel-cider-sync-response :stderr)))
       (setq org-babel-cider-sync-response
             (plist-put org-babel-cider-sync-response
                        :stderr (concat so-far err)))))
   (lambda (_buffer)
     (setq org-babel-cider-sync-response
           (plist-put org-babel-cider-sync-response :done t)))
   (lambda (_buffer ex root-ex session)
     (setq org-babel-cider-sync-response
           (plist-put org-babel-cider-sync-response :error ex)))))

(defun org-babel-cider-send-request-sync (request)
  "Send REQUEST to the nREPL server synchronously (discouraged).
  The result is a plist with keys :value, :stderr and :stdout."
  (with-current-buffer (nrepl-current-connection-buffer)
    (setq org-babel-cider-sync-response nil)
    (setq org-babel-cider-sync-request-start-time (current-time))
    (nrepl-send-request request (org-babel-cider-eval-sync-handler (current-buffer)))
    (while (or (null org-babel-cider-sync-response)
               (and (null (plist-get org-babel-cider-sync-response :done))
                    (null (plist-get org-babel-cider-sync-response :timeout))))
      (accept-process-output nil 0.005)
      ;; break out in case we don't receive a response for a while
      (when org-babel-cider-sync-request-timeout
        (let ((seconds-elapsed
               (cadr (time-subtract (current-time)
                                    org-babel-cider-sync-request-start-time))))
          (when (> seconds-elapsed org-babel-cider-sync-request-timeout)
            (setq org-babel-cider-sync-response
                  (plist-put org-babel-cider-sync-response :timeout t))))))
    org-babel-cider-sync-response))

(defun org-babel-execute:clojure (body params)
  "Evaluate a block of Clojure code with Babel."
  (let* ((result (org-babel-cider-send-request-sync
                  (nrepl--eval-request body (cider-current-ns))))
         (timeout (plist-get result :timeout))
         (value (plist-get result :value))
         (err-value (plist-get result :error))
         (out (plist-get result :stdout))
         (out (when out
                (if (string= "\n" (substring out -1))
                    (substring out 0 -1)
                  out)))
         (err (plist-get result :stderr))
         (err (when err
                (if (string= "\n" (substring err -1))
                    (substring err 0 -1)
                  err)))
         (stdout (when out
                   (mapconcat (lambda (line)
                                (concat ";; " line))
                              (split-string out "\n")
                              "\n")))
         (stderr (when err
                   (mapconcat (lambda (line)
                                (concat ";; " line))
                              (split-string err "\n")
                              "\n")))
         (output (concat stdout
                         (when (and stdout (not (string= "\n" (substring stdout -1))))
                           "\n")
                         stderr)))
    (concat output
            (when (and output
                       (not (string= "" output))
                       (not (string= "\n" (substring output -1))))
              "\n")
            (when value (concat ";;=> " value))
            (when err-value (concat ";;error= " err-value))
            (when timeout
              (concat ";; Timed out after " org-babel-cider-sync-request-timeout " seconds")))))

(provide 'ob-clojure)
;;; init-org-babel-cider.el ends here

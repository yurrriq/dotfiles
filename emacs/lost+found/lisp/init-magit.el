(require-package 'magit)

;; You have just updated to version 1.4.0 of Magit, and have to
;; make a choice.

;; Before running Git, Magit by default reverts all unmodified
;; buffers that visit files tracked in the current repository.
;; This can potentially lead to data loss, so you might want to
;; disable this by adding the following line to your init file:

(setq magit-auto-revert-mode nil)

;; The risk is not as high as it might seem.  Snapshots on MELPA
;; and MELPA-Stable have had this enabled for a long time, so if
;; you have not experienced any data loss in the past, you should
;; probably keep this enabled.

;; Keeping this mode enabled is only problematic if you, for
;; example, use `git reset --hard REV' or `magit-reset-head-hard'
;; and expect Emacs to preserve the old state of some file in a
;; buffer.  If you turn off this mode then file-visiting buffers and
;; the Magit buffer will no longer be in sync, which can be confusing
;; and would complicate many operations.  Note that it is possible
;; to undo an automatic buffer reversion using `C-x u' (`undo').

(provide 'init-magit)

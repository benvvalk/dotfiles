;;; -*- lexical-binding: t; -*-

(setq piglet-git-dir "/mnt/c/Users/Ben/git/bitbucket/piglet")
(setq piglet-viewer-git-dir "/mnt/c/Users/Ben/git/awesomesaucelabs/piglet-viewer")

(defun piglet-shell-command-to-lines (command)
  "Return the output of a shell command as a list of lines."
  (split-string (shell-command-to-string command) "\n"))

(defun piglet-git-tags-and-branches (git-dir)
  "Return a list of git tags and git branches for the repo in GIT-DIR."
  (let ((default-directory git-dir))
    (append
     (piglet-shell-command-to-lines "git tag")
     (piglet-shell-command-to-lines "git branch --format='%(refname:short)'") "\n")))

(defun piglet-unity-version-list ()
  "Return a list of installed Unity versions."
  (piglet-shell-command-to-lines "unity-list-version"))

(defun piglet-viewer-launch-unity-project ()
  "Interactive command to create and open a Unity project containing
Piglet and PigletViewer."
  (interactive)
  (let* ((unity-version (completing-read
                         "unity version: "
                         (piglet-unity-version-list)))
         (piglet-commit (completing-read
                         "piglet commit: "
                         (piglet-git-tags-and-branches
                          piglet-git-dir)))
         (piglet-viewer-commit (completing-read
                                "piglet-viewer commit: "
                                (piglet-git-tags-and-branches
                                 piglet-viewer-git-dir))))
    (async-shell-command
     (format (concat "piglet-viewer.mk "
                     "unity-version='%s' "
                     "piglet-commit='%s' "
                     "piglet-viewer-commit='%s'")
             unity-version
             piglet-commit
             piglet-viewer-commit))))

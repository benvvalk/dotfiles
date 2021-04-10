;;; -*- lexical-binding: t; -*-

(setq piglet-git-dir "/mnt/c/Users/Ben/git/bitbucket/piglet")
(setq piglet-viewer-git-dir "/mnt/c/Users/Ben/git/awesomesaucelabs/piglet-viewer")
(setq piglet-package-dir "/mnt/d/tmp/piglet/package")
(setq piglet-temp-dir "/mnt/d/tmp")

(defun piglet-git-tags-and-branches (git-dir)
  "Return a list of git tags and git branches for the repo in GIT-DIR."
  (let ((default-directory git-dir))
    (append
     (split-string (shell-command-to-string "git tag") "\n")
     (split-string (shell-command-to-string
                    "git branch --format='%(refname:short)'") "\n"))))

(defun piglet-viewer-create-package ()
  "Interactive command to create a .unitypackage for piglet-viewer.
Prompts for a piglet commit, a piglet-viewer commit, and an output
path for the resulting .unitypackage file."
  (interactive)
  (let* ((piglet-commit (completing-read
                         "piglet commit: "
                         (piglet-git-tags-and-branches
                          piglet-git-dir)))
         (piglet-viewer-commit (completing-read
                                "piglet-viewer commit: "
                                (piglet-git-tags-and-branches
                                 piglet-viewer-git-dir)))
         (default-package-name (format
                                "piglet-viewer-%s.unitypackage"
                                piglet-viewer-commit))
         (output-file (read-file-name
                       "output file: "
                       piglet-package-dir
                       default-package-name)))
    (async-shell-command
     (format (concat "create-piglet-viewer-package -v "
                     "--piglet-commit '%s' "
                     "--piglet-viewer-commit '%s' "
                     " --temp-dir '%s' --output '%s'")
             piglet-commit
             piglet-viewer-commit
             piglet-temp-dir
             output-file))))

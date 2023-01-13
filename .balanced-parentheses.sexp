;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-


(deftask prepare-release (:summary "Prepare to release a new version.")
  (format t "working dir is ~a~%" (working-directory*))
  (with-changes (:changes (changes-from-git))
    (with-semantic-version (:current (highest-git-tag))
      (setf (next-version*) (version-from-conventional-commits))
      (write-changelog-file "CHANGELOG.rst"
                            (changelog-heading :rst)
                            (changelog-lines :rst :conventional-commits
                                             :header-char "~"))

      (write-file ".git/RELEASE_CHANGELOG"
                  (format nil "Release ~a~%~%" (next-version*))
                  (changelog-lines :markdown :conventional-commits)))))

(deftask build-release-artifacts (:summary "Build the release artifacts.")
  (run-program '("python" "-m" "build")))

(deftask test-release-artifacts (:summary "Verify and test upload the artifacts.")
  (run-program "twine check dist/*")
  (run-program "twine upload -r testpypi dist/*"))

(deftask release-artifacts (:summary "Release the artifacts.")
  (run-program "twine check dist/*")
  (run-program "twine upload dist/*"))

(deftask release-new-version (:summary "Release a new version.")
  (format t "working dir is ~a~%" (working-directory*))
  (with-changes (:changes (changes-from-git))
    (with-semantic-version (:current (highest-git-tag))
      (setf (next-version*) (version-from-conventional-commits))
      (let ((message (read-file ".git/RELEASE_CHANGELOG")))
        (git-commit-changes
         :message message
         :author '(:name "Russell Sim"
                   :email "rsl@simopolis.xyz"))
        (git-create-tag
         :message message
         :author '(:name "Russell Sim"
                   :email "rsl@simopolis.xyz"))))))

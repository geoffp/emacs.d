((magit-am
  ("--3way"))
 (magit-blame
  ("-w"))
 (magit-branch nil)
 (magit-cherry-pick
  ("--ff")
  ("-x")
  nil
  ("--ff" "-x"))
 (magit-commit nil
               ("--no-verify")
               ("--allow-empty"))
 (magit-diff
  ("--no-ext-diff" "--stat")
  (("--" "client/containers/RecsContainer/RecsWithEndcap/__tests__/RecsWithEndcap.test.tsx"))
  (("--" "client/containers/RecsContainer/RecsWithEndcap/index.tsx"))
  (("--" "packages/viewer/src/PropCard/PropCard.tsx"))
  (("--" "lerna.json")))
 (magit-dispatch nil)
 (magit-fetch
  ("--prune")
  nil)
 (magit-gitignore nil)
 (magit-log
  ("-n256" "--graph" "--decorate")
  ("-n256"
   ("--" "packages/nicollet-react/src/components/Filmstrip")
   "--graph" "--decorate")
  ("-n256" "--author=Geoffrey Pursell" "--graph" "--decorate")
  ("-n256" "--author=Geoff" "--graph" "--decorate"))
 (magit-merge nil
              ("--strategy=ours"))
 (magit-merge:--strategy "ours")
 (magit-patch nil)
 (magit-pull nil)
 (magit-push nil
             ("--force-with-lease")
             ("--force")
             ("--no-verify"))
 (magit-rebase nil
               ("--autostash")
               ("--interactive")
               ("--autosquash"))
 (magit-remote
  ("-f"))
 (magit-remote\.<remote>\.*url "git@github.com:geoffp/emacs.d.git" "https://github.com/geoffp/emacs.d.git")
 (magit-reset nil)
 (magit-revert
  ("--edit")
  nil)
 (magit-stash nil
              ("--include-untracked"))
 (magit-status-jump nil)
 (magit-submodule nil)
 (magit-tag nil)
 (magit:-- "packages/nicollet-react/src/components/Filmstrip" "")
 (magit:--author "Geoffrey Pursell" "Geoff"))

function fish_prompt --description 'Write out the prompt'
	set -l last_status $status

  # User
  set_color 628797
  if set -q VIRTUAL_ENV
      echo -n -s (set_color DEB887) (basename "$VIRTUAL_ENV") "|" (set_color normal)
  end
  echo -n (whoami)
  set_color normal

  echo -n '@'

  # Host
  set_color 883930
  echo -n (hostname -s)
  set_color normal

  echo -n ':'

  # PWD
  set_color DEB887
  echo -n (prompt_pwd)
  set_color normal

  __terlar_git_prompt
  __fish_hg_prompt
  echo

  if not test $last_status -eq 0
    set_color $fish_color_error
  end

  echo -n '$ '
  set_color normal
end

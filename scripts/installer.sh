#!/bin/sh
set -e

# You can run this installer like this:
# curl https://gitlab.com/ligolang/ligo/blob/master/scripts/installer.sh | bash
# Make sure the marigold/ligo image is published at docker hub first

if test $# -ne 1; then
  printf 'Usage: installer.sh VERSION'\\n
  printf \\n
  printf '  where VERSION can be "next" or a version number like 1.0.0'\\n
  exit 1
else
  version=$1
  printf \\n'Installing LIGO (%s)'\\n\\n "$version"

  if [ $version = "next" ]
  then
    # Install the ligo.sh from master
    url=https://gitlab.com/ligolang/ligo/raw/dev/scripts/ligo.sh
  else
    # Install the ligo.sh from master
    url=https://gitlab.com/ligolang/ligo/raw/master/scripts/ligo.sh
  fi

  # Pull the docker image used by ligo.sh
  docker pull "ligolang/ligo:$version"

  # Install ligo.sh
  # Rationale behind this part of the script:
  # * mv is one of the few commands which is atomic
  #   * therefore we will create a file with the desired contents, and if that works, atomically mv it.
  #     If something goes wrong it will attempt to remove the temporary file
  #     (if removing the temporary file fails it's not a big deal due to the fairly explicit file name,
  #      the fact that it is hidden, and its small size)
  # * most utilities (e.g. touch) don't explicitly state that they support umask in their man page
  #   * therefore we try to set the mode for the temporary file with an umask + do a chmod just to be sure
  #   * this leaves open a race condition where:
  #     0) umask isn't applied by touch (e.g. the file already exists)
  #     1) for some reason touch creates an executable file (e.g. the file already exists)
  #     2) a user grabs the file while it is executable, and triggers its execution (the process is created but execution of the script doesn't start yet)
  #     3) chmod makes it non-executable
  #     4) the file is partially written
  #     5) the execution actually starts, and executes a prefix of the desired command, and that prefix is usable for adverse effects
  #     To mitigate this, we wrap the command in the script with
  #         if true; then the_command; fi
  #     That way, the shell will raise an error due to a missing "fi" if the script executed while it is partially written
  #   * This still leaves open the same race condition where a propper prefix of #!/bin/sh\nif can be used to adverse effect, but there's not much we can do about this.
  # * after the file is completely written, we make it executable
  # * we then check for the cases where `mv` misbehaves
  # * we then atomically move it to (hopefully) its destination
  # * the main risks here are if /usr/local/bin/ is writable by hostile users on the same machine (then there are bigger problems than what is our concern)
  #   or if root itself tries to create a race condition (then there are bigger problems than what is our concern)

  # It's hard to place comments inside a sequence of commands, so here are the comments for the following code:
  # wget     download to stdout
  # | sudo   become root (sudo) for the rest of the commands
  #   (      subshell (to clean up temporary file if anything goes wrong)
  #          remove temporary file in case it already exists
  #   &&     create temporary file with (hopefully) the right permissions
  #   &&     fix permisisons in case the creation didn't take umask into account
  #   &&     redirect the output of the wget download to the temporary file
  #   ) ||   clean up temporary file if any command in the previous block failed

  wget "$url" -O - \
  | sed -e "s/next/$version/g" \
  | sudo sh -c ' \
    ( \
      rm -f /usr/local/bin/.temp.ligo.before-atomic-move \
      && (umask 0600 > /dev/null 2>&1; UMASK=0600 touch /usr/local/bin/.temp.ligo.before-atomic-move) \
      && chmod 0600 /usr/local/bin/.temp.ligo.before-atomic-move \
      && cat > /usr/local/bin/.temp.ligo.before-atomic-move \
    ) || (rm /usr/local/bin/.temp.ligo.before-atomic-move; exit 1)'

  # sudo     become root (sudo) for the rest of the commands
  #   (      subshell (to clean up temporary file if anything goes wrong)
  #   &&     check that the download seems complete (one can't rely on sigpipe & failures to correctly stop the sudo session in case the download fails)
  #   &&     overwite LIGO version in the executable
  #   &&     now that the temporary file is complete, make it executable
  #   && if  check for some corner cases: destination exists and is a directory
  #   elif   check for some corner cases: destination exists and is symbolic link
  #   else   atomically (hopefully) move temporary file to its destination
  #   ) ||   clean up temporary file if any command in the previous block failed

  sudo sh -c ' \
    ( \
         grep "END OF DOWNLOADED FILE" /usr/local/bin/.temp.ligo.before-atomic-move \
      && chmod 0755 /usr/local/bin/.temp.ligo.before-atomic-move \
      && if test -d /usr/local/bin/ligo; then printf "/usr/local/bin/ligo already exists and is a directory, cancelling installation"'\\\\'n; rm /usr/local/bin/.temp.ligo.before-atomic-move; \
         elif test -L /usr/local/bin/ligo; then printf "/usr/local/bin/ligo already exists and is a symbolic link, cancelling installation"'\\\\'n; rm /usr/local/bin/.temp.ligo.before-atomic-move; \
         else mv -i /usr/local/bin/.temp.ligo.before-atomic-move /usr/local/bin/ligo; fi \
    ) || (rm /usr/local/bin/.temp.ligo.before-atomic-move; exit 1)'

  # Installation finished, try running 'ligo' from your CLI
  printf \\n'Installation successful, try to run '\''ligo --help'\'' now.'\\n
fi

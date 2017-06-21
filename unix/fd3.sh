# This shows how you can write to stdout and another file descriptor
# (here, arbitrarily chosen fd 3) simultaneously from the same
# subcommand, reading the contents of fd 3 into a variable and passing
# stdout through to the parent's stdout. Stderr is also passed through
# to the parent's stderr.
#
# I was stumped by this for at least a week or two. These finally
# helped:
#
# - http://unix.stackexchange.com/questions/18899/when-would-you-use-an-additional-file-descriptor
# - http://stackoverflow.com/questions/962255/how-to-store-standard-error-in-a-variable-in-a-bash-script
# - http://www.tldp.org/LDP/abs/html/io-redirection.html
#
# This script tested in Bash and FreeBSD /bin/sh. Are the redirection
# tricks POSIX-compliant?

echo_to_stdout_and_fd3() {
    echo "Hello stdout!"
    echo "Hello file descriptor three!" >&3
}

demo() {
    exec 4>&1  # Make fd 4 (arbitrarily chosen) an alias for stdout.
    # We can't access the parent's stdout normally via fd 1 in the
    # subcommand because $(...) binds the subcommand's fd 1 to some
    # implicit behind-the-scenes pipe that goes into the $(...)
    # expansion. But we can access the parent's stdout via the alias
    # we made on fd 4.
    local fd3_output=$(echo_to_stdout_and_fd3 3>&1 >&4)
    # Remember that we're now back in the parent shell so stdout is
    # intact. Stdout was only redirected in the subshell of $(...)
    # Fds 3 and 4 are also intact, for what it's worth.
    exec 4>&-  # Close the temp fd 4, just to be neat.
    echo "fd3_output == $fd3_output"
}

demo

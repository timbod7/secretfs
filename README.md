Secretfs is a [FUSE][] filesystem that mirrors an existing filesystem
tree, but with mustache file templates expanded and specific files
decrypted. It has been tested with OSX, and should also work on linux.

# Usage and configuration

```
secretfs

Usage: secretfs SRCDIR MOUNTDIR [--logfile ARG] [--verbose]
  Mirror a directory tree expanding templates and exposing secrets

Available options:
  -h,--help                Show this help text
  SRCDIR                   The template source tree
  MOUNTDIR                 The expanded result tree
```

All files in `SRCDIR` will be visible at the corresponding path in
`MOUNTDIR`. Special mappings for files can be configured by creating
a `.secretfs` configuration file in any directory. The `.secretfs`
file is json formatted, and contains a single object. Each key in the
object specifies the behaviour for the file with that name.
An example file would be:

```
{
    "test0"        : { "regular" : null },
    "secrets.json" : { "encrypted" : null },
    "test1"        : { "template" : "bindings.json" },
    "test2"        : { "template" : "secrets.json" }
}
```

`test0` is a regular file - it will appear in the mounted directory
exactly as in the source directory (and can be modified at any time in
either tree). Note that a `regular` specification like this is
redundant, and files not mentioned in a `.secretfs` configuration
default to this mode.

`secrets.json` is a file stored in encrypted form in the src
directory, and the corresponding cleartext is accessible in the
mounted directory. The [RNCryptor][] encryption file format is
used. The cleartext is read/write - changes to it are propagated back
to the ciphertext in the mounted directory. The tool prompts for the
decryption password on startup. If there is a "encrypted" entry in the
config, without a corresponding file in the source tree, then an empty
file will visible in the mounted tree, and the first write of
cleartext to the mounted tree will create the ciphertext in the source
tree.

`test1` is a template to be expanded. The [mustache][] template is
read from the src directory, and is expanded using values from the
specified json file, in this case `bindings.json`. The expanded
template visible in the mounted directory is readonly, however changes
made in the source template are reflected in the expanded template as
they are made.

`test2` is similar to `test1`, except that it shows that template
expansions can reference the cleartext from encrypted files.

NB: In the `.secretfs` configuration above, the keys are file names in
the current directory, whereas the argument of the "template" key
is a path relative to the root of the source tree.


# Building on osx

Use [Fuse for MacOS][osxfuse]. You can install this manually or use
[homebrew][]:

```
brew install Caskroom/cask/osxfuse
```

Then build the application using [haskell stack][stack].

```
stack build; stack install
```

[RNCryptor]:http://rncryptor.github.io/
[FUSE]:https://en.wikipedia.org/wiki/Filesystem_in_Userspace
[mustache]:https://mustache.github.io/mustache.5.html
[osxfuse]:https://osxfuse.github.io/
[homebrew]:http://brew.sh/
[stack]:https://docs.haskellstack.org/en/stable/README/

# WARNING!

This software is highly experimental. It could expose, lose or corrupt your
data. Use at your own risk, on backed up data.

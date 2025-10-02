# Emacs configuration

Random stuff that I forget if I don't write it down. Some standard key bindings, some customised inventions.

## Completion

https://github.com/minad/vertico?tab=readme-ov-file#key-bindings

##### M-RET (vertico-exit-input)

Ignore completion and exit with the minibuffer input

## Project

##### C-c q

&nbsp; Open project, add to treemacs view, visit the last visited file.

##### C-c k

&nbsp; Kill open buffers in current project, remove from treemacs view.

##### C-c f

&nbsp; Use `fd` to find files in current project. Requires `fd` to be installed. Pass command line options after a double dash `--`. Example:
     `#some-file.txt -- --hidden`

&nbsp; Use prefix argument to search in selected subdirs.

&nbsp; Read the man pages for `fd` for more info on available parameters.

##### C-c g

&nbsp; Use `rg` to grep in files in current project. Requires `rg` to be installed. Pass command line options after a double dash `--`. Example:
     
     ```
     #some-file.txt -- --glob=some-dir/** --glob=some-other-dir/** --no-ignore
     ```
     
&nbsp; Use prefix argument to grep in selected subdirs.

&nbsp; Read the man pages for `rg` for more info on available parameters.

## LSP

##### M-TAB

&nbsp; Use `company-complete` to suggest completions.

##### M-RET

&nbsp; Use `lsp-execute-code-action` to find actions opportunistically.

##### lsp-ui-sideline-mode

&nbsp; If there is too much sideline information

## Java development

### Update jdtls LSP server

1. Update the `lsp-java-jdt-download-url` from https://download.eclipse.org/jdtls/milestones/

1. Check the value of `lsp-java-server-install-dir` and delete that directory completely, to ensure a clean installation.

1. `M-x lsp-update-server`

### Useful commands

#### Create Java class using Yasnippet

1. Create the empty file, for example `my/project/Main.java`.

1. Hit `M-TAB` and select `class`, or Type 'class'. If lsp-java and yasnippet is correctly configured, an option to create the boilerplate is displayed.

## Standard commands

A few standard commands that I tend to forget.

#### Navigation and region

##### C-SPC C-SPC 

&nbsp; Set mark without activating the region

##### C-u C-SPC

&nbsp; Pop the mark (navigate back to where the mark was last set)

##### C-M-SPC

&nbsp; Activate region one expression forward

##### C-x C-x

&nbsp; Put the mark where point is now, and point where the mark is now.

#### Windows

##### C-x +

&nbsp; Balance windows

#### Find stuff

##### consult-buffer

&nbsp; Find recent buffers and files.

##### C-c g (rg)

&nbsp; Grep in project. Use prefix argument to grep in selected subdirs.

#### Misc

##### consult-theme

&nbsp; Pick a theme with consult.

##### toggle-truncate-lines

&nbsp; That’s a built-in toggle, buffer-local, and reversible.

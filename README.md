# Emacs configuration

Random stuff that I forget if I don't write it down. Some standard key bindings, some customised inventions.

## Navigation

##### C-SPC C-SPC 

&nbsp; Set mark without activating the region

##### C-u C-SPC

&nbsp; Pop the mark (navigate back to where the mark was last set)

## Project

##### C-c q

&nbsp; Open project, add to treemacs view, visit the last visited file.

##### C-c k

&nbsp; Kill open buffers in current project, remove from treemacs view.

##### C-c f

&nbsp; Use `fd` to find files in current project. Requires `fd` to be installed. Pass command line options after a double dash `--`. Example:
     `#some-file.txt -- --hidden`

&nbsp; Read the man pages for `fd` for more info on available parameters.

##### C-c g

&nbsp; Use `rg` to grep in files in current project. Requires `rg` to be installed. Pass command line options after a double dash `--`. Example:
     `#some-file.txt -- --glob=some-dir/**`

&nbsp; Read the man pages for `rg` for more info on available parameters.

## LSP

##### M-TAB

&nbsp; Use `company-complete` to suggest completions.

##### M-RET

&nbsp; Use `lsp-execute-code-action` to find actions opportunistically.

## Java development

### Update jdtls LSP server

1. Update the `lsp-java-jdt-download-url` from (https://download.eclipse.org/jdtls/milestones/)[https://download.eclipse.org/jdtls/milestones/]

1. `M-x lsp-update-server`

### Useful commands

#### Create Java class using Yasnippet

1. Create the empty file, for example `my/project/Main.java`.

1. Hit `M-TAB` and select `class`, or Type 'class'. If lsp-java and yasnippet is correctly configured, an option to create the boilerplate is displayed.

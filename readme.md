# scihub

## Introduction

Basic [SciDownl](https://pypi.org/project/scidownl/) wrapper enabling the download of files from [SciHub](https://en.wikipedia.org/wiki/Sci-Hub) within Emacs.

## Requirements

[SciDownl](https://pypi.org/project/scidownl/), as noted.

## Installation

### Manual installation

Clone this repository and add this to your `init.el` file:

``` emacs-lisp
(add-to-list 'load-path "path/to/internet-archive")
```

where `"path/to/internet-archive"` is the path to the local repository you just cloned.

### Elpaca/Straight

If you use the [elpaca](https://github.com/progfolio/elpaca) package manager, you just need to add this your `init.el` file:

``` emacs-lisp
(use-package scihub
  :elpaca (scihub
           :host github
	   :repo "benthamite/scihub")
  :demand t)
```

If you use [straight](https://github.com/radian-software/straight.el), just replace `:elpaca` with `:straight` in the formula above.

## Configuration

Set `scihub-download-directory` to the directory you would like the PDFs to be downloaded.

## Usage

`M-x scihub-download`.

## Troubleshooting

The package has not been extensively tested. If you encounter any problems, feel free to open an issue.

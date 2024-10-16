# scihub

## Introduction

Basic [SciDownl](https://pypi.org/project/scidownl/) wrapper enabling the download of files from [SciHub](https://en.wikipedia.org/wiki/Sci-Hub) within Emacs.

## Requirements

[SciDownl](https://pypi.org/project/scidownl/), as noted.

## Installation

Clone this repository to your Emacs load path and add this to your `init.el` file:

```emacs-lisp
(require 'scihub)
```

### With `use-pacakge`

If you use the [elpaca](https://github.com/progfolio/elpaca) package manager, add this your `init.el` file:

```emacs-lisp
;; with vc
(use-package scihub
  :vc (:url "https://github.com/benthamite/scihub"))

;; with elpaca
(use-package scihub
  :ensure (:host github :repo "benthamite/scihub"))

;; with straight
(use-package scihub
  :straight (:host github :repo "benthamite/scihub"))

;; with quelpa
(use-package scihub
  :quelpa (scihub :fetcher github :repo "benthamite/scihub"))
```

## Configuration

Set `scihub-download-directory` to the directory you would like the files to be downloaded.

Run `scihub-update-server-list` to update the list of available SciHub servers.

## Usage

`M-x scihub-download`.

## Troubleshooting

The package has not been extensively tested. If you encounter any problems, feel free to open an issue.

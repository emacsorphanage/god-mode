# Contributing

Please write a failing test before submitting pull requests. All new features should be accompanied by new tests.

Tests are written in [Ecukes](http://ecukes.info), an integration testing framework for Emacs.

## Setup

To fetch the test dependencies, install [cask](https://github.com/cask/cask):

```bash
curl -fsSkL https://raw.github.com/cask/cask/master/go | python
```

then:

```bash
cd /path/to/god-mode
cask
```

Run the tests with:

```bash
cask exec ecukes
```

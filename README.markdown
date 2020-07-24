[![Build Status](https://travis-ci.org/eshamster/cl-csr-2d-game.svg?branch=master)](https://travis-ci.org/eshamster/cl-csr-2d-game)

# Cl-Csr-2d-Game - A library to create 2d game using client side rendering (CSR) library.

- For client side rendering, use [cl-csr](https://github.com/eshamster/cl-csr)
- This is created from clone of [cl-web-2d-game](https://github.com/eshamster/cl-web-2d-game)
    - `cl-web-2d-game` runs on browser as JavaScript code
    - `cl-csr-2d-game` runs on server as Common Lisp code

## Installation and sample

This project and some of its dependencies are not registered on the quicklisp repository. So I recommend to use [qlot](https://github.com/fukamachi/qlot) to install them.

```bash
# Under a directory managed by quicklisp
$ git clone https://github.com/eshamster/cl-csr-2d-game.git
$ cd cl-csr-2d-game
$ ros install qlot # if you haven't installed
$ qlot install
```

After that, you can run sample as the following in REPL.

```lisp
> (ql:quickload :sample-cl-csr-2d-game)
> (sample-cl-csr-2d-game:start-sample :port 5000)
```

Then, you can see the sample by accessing to http://localhost:5000 from your browser.

## Author

- eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2019 eshamster (hamgoostar@gmail.com)

## License

Licensed under the LLGPL License.

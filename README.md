micascheme
==========

Welcome to my personal sandbox. This repository is a collection of my ongoing projects, experiments, and libraries, all built on top of **Chez Scheme**.

What's inside?
--------------

This repo is where I explore new ways of programming and interact with hardware. The most significant projects here are:

*   **Leo Scheme (/leo):** My primary focus—a programming language that brings the power of Scheme into a modern, indentation-based syntax. It’s designed for people who want the logic of Lisp without the "sea of parentheses."

*   **zx-next:** Tools for the **ZX Spectrum Next**, including a specialized assembler. It's where high-level Scheme logic meets low-level retro-hardware.

*   **Base Libraries:** Various utilities and extensions for Chez Scheme that power the rest of the experiments in this sandbox.


Getting Started
---------------

### Clone the Repository

Since this project uses submodules to manage dependencies, make sure to initialize them when you clone:

```
git clone --depth 1 https://github.com/micapolos/micascheme.git
cd micascheme
git submodule update --init --recursive --depth 1
```

### Build Leo

If you want to try out the Leo programming language right away, you can run the build script:

```
leo/build.sh
```

### Download Releases

You can find the latest stable versions and pre-built binaries here: [View Releases](https://www.google.com/search?q=https://github.com/micapolos/micascheme/releases)

Project Philosophy
------------------

Everything here is a work in progress. I use this space to bridge the gap between human-readable code and machine execution—whether that's through the vertical flow of Leo or the assembly lines of a Z80 processor.

### The Leo Syntax

Leo is designed to be a "living" document. It treats indentation as structure and uses a unique approach to data flow:

*   **Vertical Scoping:** Indentation replaces parentheses, making the logic flow downward rather than outward.

*   **Human Labeling:** It uses << and >> to clearly mark where the talking ends and the coding begins, separating your explanations from your actual logic.

*   **Literate Logic:** A .leo file can be read like a manual while remaining a valid, runnable program.


Contributing & Support
----------------------

As this is a personal sandbox, the code is often experimental and evolving quickly. However, interest and feedback are always welcome:

*   **Exploration:** Feel free to poke around the subfolders to see how the various libraries interact.

*   **Issues:** If you find a bug or have a suggestion, especially regarding Leo syntax or the zx-next assembler, please open an issue on the [GitHub tracker](https://www.google.com/search?q=https://github.com/micapolos/micascheme/issues).

*   **Support:** If you're using these tools for your own Spectrum Next projects or Scheme experiments, I'd love to hear about it!

### License

Most of the work in this repository is provided under the **MIT License**. Please check individual project subfolders for specific license files.

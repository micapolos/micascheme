# Release new version

TODO: Automate it!!!

Update `changelog.leo` with new version entry.

Update version strings in:

```
leo/version.ss
leo/man/leo.1
```

Push to github:

```
gp v0.1.xx
git tag v0.1.xx
git push origin v0.1.55
```

Wait until release is complete on Github: https://github.com/micapolos/micascheme/releases.

Update Homebrew:

```
cd ../homebrew-leo
subl .
```

Copy URL and SHA into `leo.rb`

```
gp v0.1.xx
brew update
brew upgrade
```

Done.

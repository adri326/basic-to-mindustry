# Language support for the MinBASIC language

This small vscode extension provides language support for the MinBASIC language.

To build it, run the following:

```sh
npm i -g @vscode/vsce
vsce package

# Alternatively,
npx @vscode/vsce package
```

Then, install it by running:

```sh
vscodium --install-extension ./minbasic-vscode-*.vsix

# If you're using the proprietary builds of VSCode:
vscode --install-extension ./minbasic-vscode-*.vsix
```

# Install
1. Copy file [company-english-helper.el](company-english-helper.el) and [company-english-helper-data.el](company-english-helper-data.el) to directory ~/elisp/

2. And set in your ~/.emacs like this:
```Elisp
(add-to-list 'load-path (expand-file-name "~/elisp"))
```

3. And the following to your ~/.emacs startup file.
```Elisp
(require 'company-english-helper)
```

4. Execute command `toggle-company-english-helper' to write english on the fly!

# Customize your dict.
Default english dictionary is generate from stardict KDict dictionary with below command

```Shell
python ./stardict.py stardict-kdic-ec-11w-2.4.2/kdic-ec-11w.ifo
```

You can replace with your favorite stardict dictionary's info filepath to generate your own company-english-helper-data.el .

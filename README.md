# Vim mindset apply Emacs

Truth be told, the vim community is just driven by text editing
efficiency!

They also have different modes to make cristal clear distinction when
they are editing and when they are not. On the other hand, Emacs has
some good counterparts. 

I want to document here some tricks and best practices that I found in
Vim material but backported to Emacs.

**Very important note 1: There is not, yet, a clear order to how the
material is laid out.**

**Very important note 2: I am not a good vimmer.**

**Very important note 3: I revisit the materials regularly, might
complement with time**

Invest time to sharp your editing skills!


Summary
1. [Vim Navigation Commands](#vim-navigation)
2. [Improving Vim Speed](#vim-impr-speed)
3. [Let Vim Do The Typing](#vim-do-typing)


### General tips

   1. Install [keyfreq](https://github.com/dacap/keyfreq) and regularly monitor your usage.
   2. You should not use your mouse... I know you know.. but, really, install [disable-mouse](https://github.com/purcell/disable-mouse)
   3. I keep `C-x C-s` to save my buffers, no need for that, install [auto-save](https://www.emacswiki.org/emacs/auto-save.el)
   4. Muscle memory is tough to change, keep this function around: `bk/shame-on-you`

```elisp
(defun bk/shame-on-you ()
  (interactive)
  (message "Stop this bad habbit!"))

(global-set-key (kbd "C-x C-s") #'bk/shame-on-you)
```



# 1. <a name="vim-navigation"></a> [Vim Navigation Commands](https://youtu.be/Qem8cpbJeYc)

  | Emacs Keys | Objective                                                       | Vim counterpart |
  |------------|-----------------------------------------------------------------|-----------------|
  | M-r        | Go to the last,middle,top visible line of the screen            | {H,L,G}         |
  | C-l        | Center the current line                                         |                 |
  | C-v, M-v   | Scroll without moving the cursor `(bk/scroll-{up,down})`        |                 |
  | C-a        | `bk/smart-beginning-of-line`                                    | 0               |
  | C-e        | `bk/smart-end-of-line`                                          | $               |
  | M-s .      | Search for the current word on point and look other occurrences | *               |
  | C-.        | Set a register at a specific point `(bk/point-to-register)`     | C-o             |
  | C-,        | Jump back to register specified above `(bk/jump-to-register)`   | C-i             |
  | M-n        | `(jump-char-forward)`                                           | {f,t}           |
  | M-p        | `(jump-char-backward)`                                          |                 |
  | M-{a,e}    | Move through paragraphs                                         |                 |
  | M-k        | Kill paragraph                                                  |                 |

####  Custom functions referenced above

```elisp
(defun bk/scroll-up ()
  "Scroll only specific amount of lines. I don't like the defaults of whole screen."
  (interactive)
  (scroll-up-command 8))

(defun bk/scroll-down ()
  "Scroll only specific amount of lines. I don't like the defaults of whole screen."
  (interactive)
  (scroll-down-command 8))

(defun smart-beginning-of-line ()
  "Go back at the first non-whitespace character."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
	 (beginning-of-line))))

(defun smart-end-of-line ()
  "Go to the end of the last non-whitespace character."
  (interactive)
  (move-end-of-line nil)
  (re-search-backward "^\\|[^[:space:]]")
  (forward-char))

(defun bk/point-to-register ()
  "Store cursor position in a register."
  (interactive)
  (point-to-register 8)
  (message "Point set"))

(defun bk/jump-to-register ()
  "Switches between current position and pos stored."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

```

#### External packages necessary
1. [Jump char](https://github.com/lewang/jump-char) - similar to `f` and `t` commands in Vim


# 2. <a name="vim-impr-speed"></a> [Improving Vim Speed](https://youtu.be/OnUiHLYZgaA)

| Emacs                                               | Vim                                             |
|-----------------------------------------------------|-------------------------------------------------|
| Should I use more commands to movements and editing | Stay away from insert mode                      |
| Stop using arrow keys                               | Stop using arrow keys                           |
| Use more `jump-char` to move around                 | Use more `f` and `t` to movement                |
| If you hold any character consider this a smell     | If you hold any character consider this a smell |
| **NOT CLEAR YET HOW TO HANDLE IN EMACS**            | Use the repeat command                          |
| Use fuzzy finders (learn ripgrep though)            | Use fuzzy finders                               |



# 3. <a name="vim-do-typing"></a> [Let Vim Do The Typing](https://youtu.be/3TX3kV3TICU)

| Emacs Keys       | Objectives                                               | Vim                            |
|------------------|----------------------------------------------------------|--------------------------------|
| C-i              | Complete word with previous occurrences                  | C-p                            |
| M-/              | Complete with omni-completion                            | `several keychords in ^X-mode` |
| M-\              | Complete file names `(comint-dynamic-complete-filename)` | C-x C-f (in X mode)            |
| C-c l            | Complete last line from context `(bk/try-complete-line)` | C-x C-l (in X mode)            |
| C-x r s \<char\> | Copy region into register <char> `(copy-to-register)`    |                                |
| C-x r i \<char\> | Insert text from register <char> `(insert-register)`     |                                |
| s-.              | Find tags (C-Tags)                                       |                                |

- Vim users seems to take great advantage of *registers*
- Would be nice to have tabs? (Idk, for now I am just toooo used to changing buffers with `ido`)
- Is real nice to have a `drop-down-menu` to your completions, `auto-complete` package seems more suitable
- C Tags is cool! I made the setup but I am not using that much.

You should read [Saving Text in Registers](https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Registers.html#Text-Registers)

#### Functions

```elisp
(fset 'bk/try-complete-line
      (make-hippie-expand-function '(try-expand-line)))

;;; necessary to c-tags
(setq path-to-ctags "/usr/bin/ctags")

(defun bk/create-tags (dirname)
  "Create tags file to a project located at DIRNAME."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags
	   (directory-file-name dirname))))
```


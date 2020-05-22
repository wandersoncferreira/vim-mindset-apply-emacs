# Vim tricks done in Emacs

Truth be told, the vim community is just driven by text editing
efficiency! And they have different modes to make cristal clear when
they are editing and when they are not. On the other hand, Emacs has
some good counterparts. I want to document here some tricks and best
practices that I found in Vim material but backported to Emacs.

Very important note 1: There is not, yet, a clear order to how the
material is layout.

Very important note 2: I am not a good vimmer.


# [Vim Navigation Commands](https://youtu.be/Qem8cpbJeYc)

  | Emacs Keys | Objective                                                       | Vim counterpart |
  |------------|-----------------------------------------------------------------|-----------------|
  | M-r        | Go to the last,middle,top visible line of the screen            | {H,L,G}         |
  | C-l        | Center the current line                                         |                 |
  | C-v, M-v   | Scroll without moving the cursor `(bk/scroll-{up,down})`        |                 |
  | C-a        | `bk/smart-beginning-of-line`                                    | 0               |
  | C-e        | `bk/smart-end-of-line`                                          | $               |
  | M-s .      | Search for the current word on point and look other occurrences | *               |
  | C-.        | Set a register at a specific point `(bk/point-to-register)`     |                 |
  | C-,        | Jump back to register specified above `(bk/jump-to-register)`   |                 |
  | M-n        | `(jump-char-forward)`                                           |                 |
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
1. [Jump char](https://github.com/lewang/jump-char)

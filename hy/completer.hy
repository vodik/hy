(import contextlib)
(import os)
(import re)
(import sys)

(import hy.macros)
(import hy.compiler)
(import [hy.lex.parser [mangle unmangle]])
(import [hy.-compat [builtins string-types]])

(import [builtins [eval]])

(setv docomplete True)

(try
  (import readline)
  (except [ImportError]
    (try
      (import pyreadline.rlmain)
      (import pyreadline.unicode-helper)
      (import readline)
      (except [ImportError]
        (setv docomplete False)))))

(when docomplete
  (setv readline-bind
        (if (and (= sys.platform "darwin")
                 (in "libedit" readline.--doc--))
            "bind ^I rl_complete"
            "tab: complete")))

(defclass Completer []
  (defn --init-- [self &optional [namespace {}]]
    (when (not (instance? dict namespace))
      (raise (TypeError "namespace must be a dictionary")))

    (setv self.namespace namespace
          self.path [hy.compiler.-compile-table
                     builtins.--dict--
                     (get hy.macros.-hy-macros None)
                     namespace]
          self.tag-path [(get hy.macros.-hy-tag None)])

    (setv module-name (.get namespace "__name__"))
    (when module-name
      (->> (get hy.macros.-hy-macros module-name) (.append self.path))
      (->> (get hy.macros.-hy-tag module-name) (.append self.tag-path))))

  (defn attr-matches [self text]
    (setv match (re.match r"(\S+(\.[\w-]+)*)\.([\w-]*)$" text))
    (if match
      (setv [expr attr] (map mangle (.group match 1 3)))
      (return))

    (try
      (setv obj (eval expr self.namespace)
            words (dir obj))
      (except [Exception]
        (return)))

    (for [word words]
      (when (= (cut word 0 (len attr)) attr)
        (yield (->> (, expr word)
                    (map unmangle)
                    (.join "."))))))

  (defn global-matches [self text]
    (for [path self.path
          key (.keys path)]
      (when (instance? string-types key)
        (setv key (unmangle key))
        (when (.startswith key text)
          (yield key)))))

  (defn tag-matches [self text]
    (setv text (cut text 1))
    (for [path self.tag-path
          key (.keys path)]
      (when (instance? string-types key)
        (setv key (unmangle key))
        (when (.startswith key text)
          (yield (.format "#{}" key))))))

  (defn complete [self text state]
    (setv matches
          (cond [(.startswith text "#") (self.tag-matches text)]
                [(in "." text) (self.attr-matches text)]
                [True (self.global-matches text)]))
    (try
      (return (get (list matches) state))
      (except [IndexError]
        (return None)))))

#@(contextlib.contextmanager
    (defn completion [&optional [completer None]]
      (setv delims "()[]{} ")
      (when (not completer)
        (setv completer (Completer)))

      (when docomplete
        (readline.set-completer completer.complete)
        (readline.set-completer-delims delims)
        (readline.parse-and-bind "set blink-matching-paren on")
        (readline.parse-and-bind readline-bind)

        (setv history (os.path.expanduser "~/.hy-history"))
        (try
          (readline.read-history-file history)
          (except [IOError]
            (-> (open history "a") (.close)))))

      (try
        (yield)
        (finally
          (when docomplete
            (readline.write-history-file history))))))

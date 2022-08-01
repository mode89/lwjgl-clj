(ns lwjgl-clj.gl
  (:require [lwjgl-clj.wrapper :refer (wrap-constants wrap-methods)])
  (:refer-clojure :exclude [flush]))

(wrap-constants :cls org.lwjgl.opengl.GL45 :prefix "GL_")
(wrap-methods :cls org.lwjgl.opengl.GL45 :prefix "gl")

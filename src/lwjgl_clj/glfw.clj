(ns lwjgl-clj.glfw
  (:require [lwjgl-clj.wrapper :refer (wrap-constants wrap-methods)]))

(wrap-constants :cls org.lwjgl.glfw.GLFW :prefix "GLFW_")
(wrap-methods :cls org.lwjgl.glfw.GLFW :prefix "glfw")

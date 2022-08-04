(ns lwjgl-clj-examples.vulkan
  (:require [lwjgl-clj.glfw :as glfw]
            [lwjgl-clj.vk :as vk])
  (:import org.lwjgl.vulkan.VK11
           org.lwjgl.system.MemoryUtil
           org.lwjgl.system.MemoryStack))

(defn main []
  (glfw/init)
  (glfw/window-hint glfw/CLIENT-API glfw/NO-API)
  (let [window (glfw/create-window 800 600 "Vulkan" 0 0)]
    (while (not (glfw/window-should-close window))
      (glfw/poll-events))
    (glfw/destroy-window window))
  (glfw/terminate))

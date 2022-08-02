(ns lwjgl-clj.vk
  (:require [clojure.string :as s]
            [lwjgl-clj.wrapper :refer (wrap-constants wrap-methods)])
  (:import com.google.common.reflect.ClassPath))

(wrap-constants :cls org.lwjgl.vulkan.VK11 :prefix "VK_")
(wrap-methods :cls org.lwjgl.vulkan.VK11 :prefix "vk")

(def vulkan-classes
  (->> (ClassLoader/getSystemClassLoader)
       ClassPath/from
       .getAllClasses
       (map #(.getName %))
       (filter #(s/starts-with? % "org.lwjgl.vulkan"))
       (filter #(not (s/starts-with? % "org.lwjgl.vulkan.video")))
       sort))

(def vk-classes (filter #(s/starts-with? % "org.lwjgl.vulkan.Vk")
                        vulkan-classes))

(defn make-object-of-type [type-name kwargs-list]
  (assert (even? (count kwargs-list))
          "Number of passed arguments should be even")
  (let [kwargs (->> kwargs-list
                    (partition 2)
                    (map #(into [] %))
                    (into {}))
        type-full-name (str "org.lwjgl.vulkan." type-name)
        calloc (symbol (str type-full-name "/calloc"))
        ctor-params (dissoc kwargs :stack :capacity)]
    (if (contains? kwargs :capacity)
      (do
        (assert (empty? ctor-params)
                "Can't mix :capacity with object parameters")
        (concat `(~calloc ~(:capacity kwargs))
                (if (contains? kwargs :stack)
                  (list (:stack kwargs))
                  nil)))
      (reduce (fn [acc [arg value]]
                `(~(symbol (str "." (name arg))) ~acc ~value))
              (concat `(~calloc)
                      (if (contains? kwargs :stack)
                        (list (:stack kwargs))
                        nil))
              ctor-params))))

(defn wrap-type [type-full-name]
  (let [type-name (subs type-full-name 17)
        type-abbr-name (subs type-name 2)
        type-abbr-sym (symbol type-abbr-name)]
    `(defmacro ~type-abbr-sym [& ~'params]
       (make-object-of-type ~type-name ~'params))))

(defmacro wrap-types []
  `(do ~@(map wrap-type vk-classes)))

(wrap-types)

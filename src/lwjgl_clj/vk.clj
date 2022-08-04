(ns lwjgl-clj.vk
  (:require [clojure.string :as s]
            [blancas.kern.core :as k])
  (:import com.google.common.reflect.ClassPath
           java.lang.reflect.Modifier))

(def EXT-CLASS-PREFIXES
  ["AMD"
   "ARM"
   "EXT"
   "GOOGLE"
   "HUAWEI"
   "IMG"
   "INTEL"
   "KHR"
   "NVX"
   "NV"
   "VALVE"])

(def method-name-part
  (k/<|>
    (apply k/<|> (map k/token* EXT-CLASS-PREFIXES))
    (k/token* "Win32")
    (k/bind [digits (k/many1 k/digit)]
      (k/return (s/join digits)))
    (k/bind [head k/upper
             tail (k/many k/lower)]
      (k/return (s/join (cons head tail))))))

(def method-name
  (k/bind [parts (k/many1 method-name-part)
           _ k/eof]
    (k/return parts)))

(defn method-kebab-case [meth-name]
  (let [parsed (k/parse method-name meth-name)]
    (assert (:ok parsed) (str "Failed to parse '" meth-name "'"))
    (->> (:value parsed)
         (map s/lower-case)
         (s/join \-))))

(defn const-kebab-case [const-name]
  (s/replace const-name "_" "-"))

(def VULKAN-CLASSES
  (->> (ClassLoader/getSystemClassLoader)
       ClassPath/from
       .getAllClasses
       (map #(.getName %))
       (filter #(s/starts-with? % "org.lwjgl.vulkan"))
       (filter #(not (s/starts-with? % "org.lwjgl.vulkan.video")))
       sort
       (map symbol)
       (map resolve)))

(def VK-CLASSES (filter #(s/starts-with? (.getSimpleName %) "Vk")
                        VULKAN-CLASSES))

(defn gen-class-instantiation [cls kwargs-list]
  (assert (even? (count kwargs-list))
          "Number of passed arguments should be even")
  (let [kwargs (->> kwargs-list
                    (partition 2)
                    (map #(into [] %))
                    (into {}))
        cls-name (.getName cls)
        calloc (symbol (str cls-name "/calloc"))
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

(defn gen-class-wrapper [cls]
  (let [cls-simp-name (.getSimpleName cls)
        cls-wrap-name (subs cls-simp-name 2)
        cls-wrap-sym (symbol cls-wrap-name)]
    `(defmacro ~cls-wrap-sym [& ~'params]
       (gen-class-instantiation ~cls ~'params))))

(defmacro wrap-classes [classes]
  `(do ~@(map gen-class-wrapper (eval classes))))

(defn is-ext-class [cls-name]
  (some #(s/starts-with? cls-name %) EXT-CLASS-PREFIXES))

(def EXT-CLASSES (filter #(is-ext-class (.getSimpleName %))
                         VULKAN-CLASSES))

(defn class-methods [prefix cls]
  (->> cls
       .getMethods
       (filter #(Modifier/isPublic (.getModifiers %)))
       (filter #(Modifier/isStatic (.getModifiers %)))
       (map #(.getName %))
       (filter #(s/starts-with? % prefix))))

(defn gen-method-call [meth-name args]
  `(~(symbol meth-name) ~@args))

(defn gen-method-wrapper [prefix cls-name meth-short-name]
  (let [meth-wrap-name (method-kebab-case
                         (subs meth-short-name (count prefix)))
        meth-name (str cls-name "/" meth-short-name)]
    `(defmacro ~(symbol meth-wrap-name) [& ~'params]
       (gen-method-call ~meth-name ~'params))))

(defn gen-class-methods-wrappers [prefix cls]
  (let [cls-name (.getName cls)]
    (map #(gen-method-wrapper prefix cls-name %)
         (class-methods prefix cls))))

(defmacro wrap-methods [prefix classes]
  `(do
     ~@(mapcat #(gen-class-methods-wrappers prefix %)
               (eval classes))))

(defn class-constants [prefix cls]
  (->> cls
       .getFields
       (filter #(Modifier/isPublic (.getModifiers %)))
       (filter #(Modifier/isStatic (.getModifiers %)))
       (map #(.getName %))
       (filter #(s/starts-with? % prefix))))

(defn gen-constant-wrapper [prefix cls-name const-name]
  (let [const-abbr-name (subs const-name (count prefix))
        const-wrap-name (s/replace const-abbr-name "_" "-")]
    `(def ~(symbol const-wrap-name)
          ~(symbol (str cls-name "/" const-name)))))

(defn gen-class-constants-wrappers [prefix cls]
  (let [cls-name (.getName cls)]
    (map #(gen-constant-wrapper prefix cls-name %)
         (class-constants prefix cls))))

(defmacro wrap-constants [prefix classes]
  `(do
     ~@(mapcat #(gen-class-constants-wrappers prefix %)
               (eval classes))))

(wrap-classes VK-CLASSES)
(wrap-methods "vk" (cons org.lwjgl.vulkan.VK11 EXT-CLASSES))
(wrap-constants "VK_" (cons org.lwjgl.vulkan.VK11 EXT-CLASSES))

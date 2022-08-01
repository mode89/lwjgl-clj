(ns lwjgl-clj.wrapper
  (:require [clojure.string :as s]
            [clojure.reflect :refer [reflect typename]])
  (:refer-clojure :exclude [flush]))

(defn make-wrapper-method-name
  ([nom] (make-wrapper-method-name nom false))
  ([nom allow-hyphen]
     (let [head (first nom)
           tail (rest nom)]
       (cond
         (empty? nom) ""
         (Character/isDigit head)
           (str (if allow-hyphen \- "")
                head
                (make-wrapper-method-name tail false))
         (not allow-hyphen)
           (str (Character/toLowerCase head)
                (make-wrapper-method-name tail true))
         (Character/isUpperCase head)
           (str \- (Character/toLowerCase head)
                (make-wrapper-method-name tail true))
         :else
           (str head (make-wrapper-method-name tail true))))))

(defn make-wrapper-const-name [nom]
  (s/replace nom "_" "-"))

(defn list-hierarchy [cls]
  (if (nil? cls)
    '()
    (let [parent (first (bases cls))]
      (conj (list-hierarchy parent) cls))))

(defn list-methods [prefix cls]
  (->> (reflect cls)
       :members
       (map :name)
       (filter #(s/starts-with? %1 prefix))
       (sort)
       (distinct)))

(defn list-all-methods [prefix cls]
  (->> (list-hierarchy cls)
       (mapcat #(list-methods prefix %1))
       (sort)
       (distinct)))

(defn wrap-method [& {:keys [prefix class-name method-name]}]
  (let [name-no-prefix (subs method-name (count prefix))
        sname (symbol (make-wrapper-method-name name-no-prefix))
        smethod (symbol (str class-name "/" method-name))]
    `(defmacro ~sname [& ~'params]
       `(~'~smethod ~@~'params))))

(defn list-consts [prefix cls]
  (->> (reflect cls)
       :members
       (filter #(not (contains? % :return-type)))
       (map :name)
       (filter #(s/starts-with? %1 prefix))
       (sort)))

(defn list-all-consts [prefix cls]
  (->> (list-hierarchy cls)
       (mapcat #(list-consts prefix %1))
       (sort)
       (distinct)))

(defn wrap-constant [& {:keys [prefix class-name const-name]}]
  (let [cname-no-prefix (subs const-name (count prefix))
        wname (symbol (make-wrapper-const-name cname-no-prefix))
        qname (symbol (str class-name "/" const-name))]
    `(def ~wname ~qname)))

(defmacro wrap-constants [& {:keys [prefix cls]}]
  `(do
     ~@(map #(wrap-constant :prefix prefix
                            :class-name (typename cls)
                            :const-name (str %1))
            (list-all-consts prefix (resolve cls)))))

(defmacro wrap-methods [& {:keys [prefix cls]}]
  `(do
     ~@(map #(wrap-method :prefix prefix
                          :class-name (typename cls)
                          :method-name (str %1))
            (list-all-methods prefix (resolve cls)))))

(ns openapi-spec-provider.core
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [clojure.walk :as walk]
   [clojure.pprint :as pprint]
   [clj-yaml.core :as yaml]
   [camel-snake-kebab.core :as csk]))


(declare schema->spec-defs)

;;;; Simple

(defn ^:private simple?
  "A simple schema can be defined with a single spec def."
  [{:keys [type] :as schema}]
  (boolean
   (#{"string" "number" "integer" "boolean"} type)))

(defmulti schema-simple->spec :type)

(defmethod schema-simple->spec :default
  [schema]
  (throw (ex-info "Unsupported schema" schema)))


;;; String

(defmulti string-format->spec identity)
(defmethod string-format->spec :date [_] nil)
(defmethod string-format->spec :date-time [_] nil)
(defmethod string-format->spec :password [_] nil)
(defmethod string-format->spec :byte [_] nil)
(defmethod string-format->spec :binary [_] nil)
(defmethod string-format->spec :default [_] nil)

(defn schema-string->spec
  [{:keys [type format pattern minLength maxLength]
    :as   schema}]
  (let [format-spec  (when format (string-format->spec format))
        pattern-spec (when pattern
                       `(fn [~'s] (re-find (re-pattern ~pattern) ~'s)))
        min-spec     (when minLength
                       `(fn [~'s] (<= ~minLength (count ~'s))))
        max-spec     (when maxLength
                       `(fn [~'s] (<= (count ~'s) ~maxLength)))
        modifiers    (remove nil? [format-spec pattern-spec min-spec max-spec])]

    (if (not (empty? modifiers))
      (concat `(s/and string?) modifiers)
      `string?)))


(defmethod schema-simple->spec "string"
  [schema]
  (schema-string->spec schema))


;;; Boolean

(defmethod schema-simple->spec "boolean" [schema]
  `boolean?)


;;; Number and Integer

(defmulti number-format->spec identity)
(defmethod number-format->spec "float" [_] `float?)
(defmethod number-format->spec "double" [_] `double?)
;; NOTE: int types are an unnecessary format specifier
(defmethod number-format->spec "int32" [_] nil)
(defmethod number-format->spec "int64" [_] nil)

(defn schema-number->spec
  [{:keys [type format
           minimum exclusiveMinimum
           maximum exclusiveMaximum
           multipleOf]
    :as   schema}]
  (let [type-spec     (case type
                        "number"  `number?
                        "integer" `int?)
        format-spec   (when format (number-format->spec format))
        min-spec      (when minimum
                        (let [comparator (if exclusiveMinimum `< `<=)]
                          `(fn [~'x]
                             (~comparator ~minimum ~'x))))
        max-spec      (when maximum
                        (let [comparator (if exclusiveMaximum `< `<=)]
                          `(fn [~'x]
                             (~comparator ~'x ~maximum))))
        multiple-spec (when multipleOf
                        `(fn [~'x]
                           (zero? (rem ~'x ~multipleOf))))
        modifiers     (remove nil? [format-spec min-spec max-spec multiple-spec])]
    (if (not (empty? modifiers))
      (concat `(s/and ~type-spec) modifiers)
      type-spec)))

(defmethod schema-simple->spec "number"
  [schema]
  (schema-number->spec schema))

(defmethod schema-simple->spec "integer"
  [schema]
  (schema-number->spec schema))

;;;; Ref

(defn ^:private ref?
  [schema]
  (contains? schema :$ref))

(def ^:dynamic *root-ns* nil)

(defn ^:private ref->key
  "Convert a ref string to a spec key."
  [r]
  (let [key (-> r
                (string/split #"\/")
                last
                ;(csk/->kebab-case)
                )]
    (keyword (name *root-ns*) key)))

(defn schema-ref->spec
  [schema]
  (ref->key (:$ref schema)))

(defn ^:private subfield-symbol
  "Given a namespaced keword and a name, return a keyword
  that can be used as a spec name for a spec under the given keyword/symbol."
  [k field]
  (let [ns (str (namespace k) "." (name k))]
    (keyword ns
             ;(csk/->kebab-case (name field))
             (name field)
             )))


;;;; Array

(defn schema-array->spec-defs
  [schema kdef]
  (let [{:keys [items minItems maxItems uniqueItems]}
        schema
        qualify     (partial subfield-symbol kdef)
        min-spec    (when minItems
                      `(fn [~'coll] (<= ~minItems (count ~'coll))))
        max-spec    (when maxItems
                      `(fn [~'coll] (<= (count ~'coll) ~maxItems)))
        unique-spec (when uniqueItems
                      `(fn [~'coll] (apply distinct? ~'coll)))
        modifiers   (remove nil? [min-spec max-spec unique-spec])
        predefs     (when (and (not (simple? items))
                               (not (ref? items)))
                      (schema->spec-defs items (qualify "item")))
        type-spec   (cond
                      (simple? items) (schema-simple->spec items)
                      (ref? items)    (schema-ref->spec items)
                      :else           (qualify "item"))
        ]
    (concat predefs
            `((s/def ~kdef
                ~(if (not (empty? modifiers))
                   (concat `(s/and (s/coll-of ~type-spec)) modifiers)
                   `(s/coll-of ~type-spec)))))
    ))


;;;; Object

(defn schema-object->spec-defs
  [schema kdef]
  (let [{:keys [properties required]} schema
        qualify (partial subfield-symbol kdef)
        props   (->> properties keys (mapv name))
        reqs    required
        opts    (->> props (remove (set reqs)) vec)
        predefs (reduce-kv
                 (fn [acc k v]
                   (concat acc (schema->spec-defs v (qualify k))))
                 '()
                 properties)]
    (concat predefs
            `((s/def ~kdef
                (s/keys
                 ~@(when (seq reqs)
                     [:req-un (mapv qualify reqs)])
                 ~@(when (seq opts)
                     [:opt-un (mapv qualify opts)])
                 ))))))

(defn ^:private nilable
  [spec-defs]
  (let [spec-def (last spec-defs)
        pre      (vec (butlast spec-def))
        form     (last spec-def)
        nildef   (seq (conj pre `(s/nilable ~form)))]
    (concat (butlast spec-defs)
            (list nildef))))

(defn schema->spec-defs
  [schema kdef]
  (let [{:keys [nullable type]} schema
        defs (cond
               (simple? schema)
               `((s/def ~kdef ~(schema-simple->spec schema)))

               (= "array" type)
               (schema-array->spec-defs schema kdef)

               (= "object" type)
               (schema-object->spec-defs schema kdef)

               (ref? schema)
               `((s/def ~kdef ~(schema-ref->spec schema)))
               )]
    (cond-> defs
      nullable nilable)))


;; Utils

(defn unqualify-ns
  [form ns]
  (let [ns (str ns)]
    (walk/postwalk
     (fn [x]
       (cond (and (symbol? x) (= ns (namespace x)))
             (symbol (name x))

             (and (keyword? x) (= ns (namespace x)))
             (symbol (str "::" (name x)))

             :else x))
     form)))

(defn ns-as
  [form old-ns new-ns]
  (let [old-ns (and old-ns (str old-ns))
        new-ns (and new-ns (str new-ns))]
    (walk/postwalk
     (fn [x]
       (cond (and (symbol? x) (= old-ns (namespace x)))
             (symbol new-ns (name x))

             (and (keyword? x) (= old-ns (namespace x)))
             (keyword new-ns (name x))

             :else x))
     form)))

(defn schemas->spec-defs
  [schemas root-ns]
  (binding [*root-ns* root-ns]
    (reduce-kv
     (fn [acc k v]
       (let [kdef (keyword (name *root-ns*)
                           ;(csk/->kebab-case (name k))
                           (name k))
             defs (-> (schema->spec-defs v kdef)
                      (ns-as 'clojure.core nil)
                      (ns-as 'clojure.spec.alpha 's))]
         (into (conj acc (symbol "\r")) defs)))
     []
     schemas)))

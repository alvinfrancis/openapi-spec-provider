(ns openapi-spec-provider.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [openapi-spec-provider.core :refer :all]))

(deftest simple-schemas
  (testing "string"
    (let [spec (eval (schema-simple->spec {:type "string"}))]
      (is (s/valid? spec "test"))
      (is (not (s/valid? spec 3)))
      (is (not (s/valid? spec true)))
      (is (not (s/valid? spec :test))))

    (testing "length"
      (let [spec (eval (schema-simple->spec {:type      "string"
                                             :minLength 3}))]
        (is (s/valid? spec "test"))
        (is (not (s/valid? spec "te"))))

      (let [spec (eval (schema-simple->spec {:type      "string"
                                             :minLength 3}))]
        (is (s/valid? spec "test"))
        (is (not (s/valid? spec "te"))))

      (let [spec (eval (schema-simple->spec {:type      "string"
                                             :maxLength 3}))]
        (is (not (s/valid? spec "test")))
        (is (s/valid? spec "te")))

      (let [spec (eval (schema-simple->spec {:type      "string"
                                             :minLength 3
                                             :maxLength 5}))]
        (is (s/valid? spec "test"))
        (is (not (s/valid? spec "te")))
        (is (not (s/valid? spec "tested")))))

    (testing "pattern"
      (let [spec (eval (schema-simple->spec {:type      "string"
                                             :minLength 4
                                             :pattern   "[a-z]*"}))]
        (is (s/valid? spec "test"))
        (is (not (s/valid? spec "tes")))
        (is (s/valid? spec "tes1")))

      (let [spec (eval (schema-simple->spec {:type      "string"
                                             :minLength 4
                                             :pattern   "^[a-z]*$"}))]
        (is (s/valid? spec "test"))
        (is (not (s/valid? spec "tes1"))))))

  (testing "number"
    (let [spec (eval (schema-simple->spec {:type "number"}))]
      (is (not (s/valid? spec "test")))
      (is (s/valid? spec 3))
      (is (s/valid? spec 3.5))
      (is (not (s/valid? spec true)))
      (is (not (s/valid? spec :test))))

    (testing "range"
      (let [spec (eval (schema-simple->spec {:type    "number"
                                             :minimum 1
                                             :maximum 20}))]

        (is (s/valid? spec 1))
        (is (s/valid? spec 1.1))
        (is (s/valid? spec 3.5))
        (is (not (s/valid? spec 0.9)))
        (is (not (s/valid? spec 20.1))))

      (testing "exlusivity"
        (let [spec (eval (schema-simple->spec {:type             "number"
                                               :minimum          1
                                               :exclusiveMinimum true
                                               :exclusiveMaximum true
                                               :maximum          20}))]

          (is (not (s/valid? spec 1)))
          (is (not (s/valid? spec 20)))
          (is (s/valid? spec 1.1))
          (is (s/valid? spec 3.5))
          (is (not (s/valid? spec 0.9)))
          (is (not (s/valid? spec 20.1))))))

    (testing "multipleOf"
      (let [spec (eval (schema-simple->spec {:type "number"
                                             :multipleOf 2.5}))]
        (is (s/valid? spec 2.5))
        (is (s/valid? spec 5))
        (is (s/valid? spec -2.5))
        (is (s/valid? spec -5))
        (is (s/valid? spec 0))
        (is (not (s/valid? spec 3)))
        (is (not (s/valid? spec 3.5)))))))

(ns unfollow.core-test
  (:require
      [cljs.test      :refer [deftest is testing use-fixtures]]
      [clojure.string :as str]
      [unfollow.core  :as core]))


(def MOCK_MASTODON_INSTANCE_URL "https://my.custom.instance.com")
(def MOCK_MASTODON_ACCESS_TOKEN "ZA-Yj3aBD8U8Cm7lKUp-lm9O9BmDgdhHzDeqsY8tlL0")


(use-fixtures :each
  (fn [t]
    (let [saved (js/Object.assign #js {} js/process.env)]
      ;; load required vars
      (set! js/process.env.MASTODON_INSTANCE_URL MOCK_MASTODON_INSTANCE_URL)
      (set! js/process.env.MASTODON_ACCESS_TOKEN MOCK_MASTODON_ACCESS_TOKEN)

      ;; execute test
      (t)

      ;; restore previous environment state
      (set! js/process.env saved))))


(deftest read-config
  ;; NOTE: Don't change test order
  (testing "Config value extraction"
    (testing "with all required environment variables present"
      (let [{:keys [ok error]} (core/read-config)]
        (is (nil? error))
        (is (not (str/blank? (:instance  ok))))
        (is (not (str/blank? (:token     ok))))))

    (testing "with a single one (token) missing"
      ;; remove token loaded as part of fixture setup
      (js-delete js/process.env "MASTODON_ACCESS_TOKEN")
      (let [{:keys [ok error]} (core/read-config)]
        (is (nil? ok))
        (is (= ["MASTODON_ACCESS_TOKEN"] error))))))


(deftest parse-link
  ;; NOTE: For a short overview of how pagination works in Mastodon and
  ;;       what role the HTTP links header as therein refer to the
  ;;       [pagination section](https://docs.joinmastodon.org/api/guidelines/#pagination)) of the guidelines.
  (testing "HTTP link header parsing for pagination"
    (let [next-link-val "https://mastodon.example/api/v1/endpoint?max_id=7163058"
          prev-link-val "https://mastodon.example/api/v1/endpoint?min_id=7275607"
          next-link     (str "<" next-link-val ">; rel=\"next\"")
          prev-link     (str "<" prev-link-val ">; rel=\"prev\"")]

      (testing "with both previous page and next page links."
        (let [link    (str next-link ", " prev-link)
              parsed  (core/parse-link link)]
          (is (contains? parsed :prev))
          (is (contains? parsed :next))
          (is (= prev-link-val (:prev parsed)))
          (is (= next-link-val (:next parsed)))))

      (testing "with only a previous page link"
        (let [link    prev-link
              parsed  (core/parse-link link)]
          (is (contains? parsed :prev))
          (is (= prev-link-val (:prev parsed)))))

      (testing "with only a next page link."
        (let [link    next-link
              parsed  (core/parse-link link)]
          (is (contains?  parsed :next))
          (is (= next-link-val (:next parsed)))))

      (testing "with an empy string."
        (let [link   ""
              parsed (core/parse-link link)]
          (is (nil? parsed)))))))


(deftest fetch-page
  (testing "Handling of a response with a non-successful HTTP status code."))

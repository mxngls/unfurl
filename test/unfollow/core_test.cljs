(ns unfollow.core-test
  (:require
      [cljs.test      :refer [deftest is testing use-fixtures]]
      [clojure.string :as str]
      [unfollow.core  :as core]))


(def MOCK_MASTODON_INSTANCE_URL  "https://my.custom.instance.com")
(def MOCK_MASTODON_API_KEY  "ZA-Yj3aBD8U8Cm7lKUp-lm9O9BmDgdhHzDeqsY8tlL0")


(use-fixtures :each
  (fn [t]
    (let [saved (js/Object.assign #js {} js/process.env)]
      ;; load required vars
      (set! js/process.env.MASTODON_INSTANCE_URL MOCK_MASTODON_INSTANCE_URL)
      (set! js/process.env.MASTODON_API_KEY      MOCK_MASTODON_API_KEY)

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
      (js-delete js/process.env "MASTODON_API_KEY")
      (let [{:keys [ok error]} (core/read-config)]
        (is (nil? ok))
        (is (= ["MASTODON_API_KEY"] error))))))

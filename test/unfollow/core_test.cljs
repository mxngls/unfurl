(ns unfollow.core-test
  (:require
      [cljs.core.async  :refer [go]]
      [cljs.test        :refer [async deftest is testing use-fixtures]]
      [clojure.core.async :refer [<!]]
      [clojure.string   :as str]
      [unfollow.core    :as core]))


(def MOCK_MASTODON_INSTANCE_URL "https://my.custom.instance.com")
(def MOCK_MASTODON_ACCESS_TOKEN "ZA-Yj3aBD8U8Cm7lKUp-lm9O9BmDgdhHzDeqsY8tlL0")


(defn ->mock-response
  ([] (->mock-response nil {}))
  ([body] (->mock-response body {}))
  ([body {:keys [status headers]
          :or   {status 200 headers {}}}]
   (js/Response. body #js {:status status :headers (clj->js headers)})))


(def orig-fetch (.-fetch js/globalThis))


(defn mock-fetch!
  [impl]
  (set!
      (.-fetch js/globalThis)
      (if (fn? impl)
        impl
        (fn [& _] (js/Promise.resolve impl)))))


(defn restore-fetch!
  []
  (set! (.-fetch js/globalThis) orig-fetch))


(def mock-url "https://mastodon.example/api/v1/endpoint")

(def ^:private saved-env (atom nil))


(use-fixtures :each
  {:before (fn []
             (reset! saved-env (js/Object.assign #js {} js/process.env))
             (set! js/process.env.MASTODON_INSTANCE_URL MOCK_MASTODON_INSTANCE_URL)
             (set! js/process.env.MASTODON_ACCESS_TOKEN MOCK_MASTODON_ACCESS_TOKEN))
   :after   (fn []
              (set! js/process.env @saved-env))})


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


(deftest fetch-raw
  (async done
         (go
           (try

             (testing "Handling 200 HTTP response"
               (mock-fetch! (->mock-response nil {:status 200}))
               (let [{:keys [ok error]} (<! (core/fetch-raw mock-url))]
                 (is (nil? error))
                 (is (.-ok ok))))

             (testing "Handling a non-200 HTTP response"
               (mock-fetch! (->mock-response nil {:status 404}))
               (let [{:keys [ok error]} (<! (core/fetch-raw mock-url))]
                 (is (nil? ok))
                 (is (= 404 (.-status error)))))

             (testing "Handling an error non-HTTP error"
               (mock-fetch! (fn [_ _] (js/Promise.reject (js/Error. "no response"))))
               (let [{:keys [ok error]} (<! (core/fetch-raw mock-url))]
                 (is (nil? ok))
                 (is (some? error))))

             (finally
               (restore-fetch!)
               (done))))))


(deftest fetch-page
  (async done
         (let [;; obtained from https://docs.joinmastodon.org/methods/accounts/#200-ok-6
               mock-accounts
               "[{\"id\": \"1020382\", \"username\": \"atul13061987\", \"acct\": \"atul13061987\", \"display_name\": \"\"},
                 {\"id\": \"1020381\", \"username\": \"linuxliner\", \"acct\": \"linuxliner\", \"display_name\": \"\"}]"]
           (go
             (try

               (testing "fetching a page successfully"
                 (mock-fetch! (->mock-response mock-accounts
                                               {:status 200
                                                :headers {:Content-Type "application/json"}}))
                 (let [{:keys [ok error]} (<! (core/fetch-page mock-url))
                       items              (:items ok)]
                   (is (nil? error))

                   (is items)
                   (is (nil? (:next ok)))
                   (is (nil? (:prev ok)))
                   (is (= "1020382" (:id (first  items))))
                   (is (= "1020381" (:id (second items))))))

               (testing "fetching a page whose body is not encoded in JSON"
                 (mock-fetch! (->mock-response "Hello, World!"
                                               {:status 200
                                                :headers {:Content-Type "text/plain"}}))
                 (let [{:keys [ok error]} (<! (core/fetch-page mock-url))]
                   (is (nil? ok))
                   (is (instance? js/Response error))))

               (testing "fetching a page but failing to parse it's body"
                 (mock-fetch! (->mock-response 123123123123
                                               {:status 200}))
                 (let [{:keys [ok error]} (<! (core/fetch-page mock-url))]
                   (is (nil? ok))
                   (is (instance? js/Response error))))

               (testing "fetching a page but encountering an error"
                 (mock-fetch! (fn [_ _] (js/Promise.reject (js/Error. "no response"))))
                 (let [{:keys [ok error]} (<! (core/fetch-raw mock-url))]
                   (is (nil? ok))
                   (is (instance? js/Response error))))

               (finally
                 (restore-fetch!)
                 (done)))))))

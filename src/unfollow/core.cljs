(ns unfollow.core
  (:require
      [cljs.core.async          :refer [<! go go-loop timeout]]
      [cljs.core.async.interop  :refer-macros [<p!]]
      [clojure.string :as str]
      [goog.object :as gobj]))


;; Documentation to be found at: 
;; * Unfollowing: https://docs.joinmastodon.org/methods/accounts/#unfollow
;; * Following:   https://docs.joinmastodon.org/methods/accounts/#following
;; * Credentials: https://docs.joinmastodon.org/methods/accounts/#verify_credentials

(def log      js/console.log)
(def log-err  js/console.error)


(defn read-config
  "Returns a result of either all required environment variables or a
   error containing a list of the ones that are missing."
  []
  (let [instance  (gobj/get js/process.env "MASTODON_INSTANCE_URL")
        token     (gobj/get js/process.env "MASTODON_ACCESS_TOKEN")
        missing   (cond-> []
                    (str/blank? instance) (conj "MASTODON_INSTANCE_URL")
                    (str/blank? token)    (conj "MASTODON_ACCESS_TOKEN"))]
    (if (seq missing)
      {:error missing}
      {:ok    {:instance instance :token token}})))


(defn load-config!
  "Returns the config read from environment variables or instead logs
   an error message and exits."
  []
  (let [{:keys [ok error]} (read-config)]
    (if error
      (do (log-err (str "missing required environment variable(s):\n"
                        (str/join "\n" (map #(str "- " %) error))))
          (js/process.exit 1))
      ok)))


(def config (delay (load-config!)))


(defn parse-link
  "Parse link header according to RFC 8288. We limit ourselves to the 
   shape described in [Paginating through API responses](https://docs.joinmastodon.org/api/guidelines/#pagination).
   
   Returns a map from each link's `rel` (keywordized) to its URL, or
   nil."
  [header]
  (not-empty
      (into {} (for [link (str/split header #",\s*")
                     :let   [rel  (second  (re-find #"rel=\"([^\"]+)\"" link))
                             url  (second  (re-find #"<([^>]+)>" link))]
                     :when  (and rel url)]
                 [(keyword rel) url]))))


(defn fetch-raw
  "Generic wrapper around Javascript's fetch method with some error
   handling and custom header parsing.
  
   Used across different endpoints handling more targeted fetch calls.
   
   Returns a response result."
  [url & {:keys [wait method fetch-fn]
          :or   {wait 3000
                 method "GET"
                 fetch-fn (fn [url opts] (js/fetch url opts))}}]
  (go
    (when (pos? wait)
      (<! (timeout wait)))
    (try
      (let [res           (<p! (fetch-fn url #js {:headers #js
                                                           {:Authorization (str "Bearer " (:token @config))}
                                                  :method  method}))]
        (if (.-ok res)
          {:ok res}
          (do  (log-err "HTTP error:" res)
               {:error res})))

      (catch :default e
        (log-err "failed to fetch page:" e) {:error e}))))


(defn fetch-page
  "Returns a result which in the successful case we it contains a map of:

   1. items:  a vector of items for this page (an items shape depends on
              the provided endpoint)
   2. next:   URL of the next page — older entries (smaller ids), or nil
   3. prev:   URL of the previous page — newer entries (larger ids), or 
              nil

   In case an error occured the we return the error as is. If the result
   of the fetch call is otherwise not processable we return the response
   object received as is."
  [url & opts]
  (go
    (try
      (let [{:keys [ok error]} (<! (apply fetch-raw url opts))]
        (if error

          error

          (let [headers       (.-headers ok)
                body          (<p! (.text ok))
                content-type  (or (.get headers "content-type") "")]

            (if (not (str/includes? content-type "json"))
              (do (log-err "expected JSON got" (pr-str content-type) "\n" body)
                  {:error ok})

              (let [data  (js/JSON.parse body)
                    arr   (if (array? data) data #js [data])
                    links (some-> (.get headers "link") parse-link)]

                {:ok {:items (js->clj arr :keywordize-keys true)
                      :next  (:next links)
                      :prev  (:prev links)}})))))

      (catch :default e
        (log-err "failed to read/parse body:" e)
        {:error e}))))


(defn get-self
  "Returns a result containing the `Account` map for the account 
   associated with the provided authorization token, or `nil.`"
  []
  (go
    (let [url                 (str (:instance @config) "/api/v1/accounts/verify_credentials")
          {:keys [ok error]}  (<! (fetch-page url))]
      (if error
        {:error error}
        {:ok (-> ok
                 :items
                 first)}))))


(defn get-following
  "Returns a result containing a single page where \"items\" is 
   populated with `Account` maps for a given account associated 
   with the provided id that this account is following."
  [id & [query_params]]
  (go
    (let [params    (merge {:limit 80} query_params)
          qs        (str "?" (.toString (js/URLSearchParams. (clj->js params))))
          url       (str (:instance @config) "/api/v1/accounts/" id "/following" qs)
          {:keys [ok error]} (<! (fetch-page url))]
      (if error
        {:error error}
        {:ok    ok}))))


(defn get-following-all
  "Returns a result containing the vector of all collected `Accounts` 
   across pages the user with the associated account id is following."
  [id]
  (go-loop [{:keys [ok error]} (<! (get-following id))
            accounts []]

    (if error
      {:error error}
      (let [following  (into accounts (:items ok))
            next-url   (:next ok)]
        (if next-url
          (recur (<! (fetch-page next-url)) following)
          {:ok following})))))


(defn unfollow
  "Unfollows the account associated with the provided id.
  
   Returns a result containing a `Relationship` map."
  [id]
  (go
    (let [url                 (str (:instance @config) "/api/v1/accounts/" id "/unfollow")
          {:keys [ok error]}  (->  (<! (fetch-page url {:method "POST" :wait 500})))]
      (if error
        {:error error}
        (first {:items ok})))))


(defn main
  [& _args]
  ;; eagerly evalute config
  @config
  (go
    (when-let [account (<! (get-self))]
      (let [id       (:id account)
            accounts (<! (get-following-all id))]

        (doseq [acc accounts]
          (let [{:keys [_ error]}  (<! (unfollow (:id acc)))
                acc-name            (:display_name acc)]

            (if error
              (log-err (str "Failed to delete account:" acc-name "\n" error))

              (log     (str "Successfully unfollowed:"  acc-name)))))))))

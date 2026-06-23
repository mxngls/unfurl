(ns unfollow.core
  (:require
      [cljs.core.async :refer [<! go go-loop timeout]]
      [cljs.core.async.interop :refer-macros [<p!]]
      [clojure.string :as str]
      [goog.object :as gobj]))


;; Documentation to be found at: 
;; * Unfollowing: https://docs.joinmastodon.org/methods/accounts/#unfollow
;; * Following:   https://docs.joinmastodon.org/methods/accounts/#following
;; * Credentials: https://docs.joinmastodon.org/methods/accounts/#verify_credentials

(def _log      js/console.log)
(def log-err  js/console.error)


(defn read-config
  "Returns a result of either all required environment variables or a 
   error containing a list of the ones that are missing."
  []
  (let [instance  (gobj/get js/process.env "MASTODON_INSTANCE_URL")
        token     (gobj/get js/process.env "MASTODON_API_KEY")
        missing   (cond-> []
                    (str/blank? instance) (conj "MASTODON_INSTANCE_URL")
                    (str/blank? token)    (conj "MASTODON_API_KEY"))]
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
  "Parse link header according to RFC 8288. We limit ourselves to the shape described in
   [Paginating through API responses](https://docs.joinmastodon.org/api/guidelines/#pagination).
   
   Returns a map from each link's `rel` (keywordized) to its URL, or nil."
  [header]
  (into {} (for [link (str/split header #",\s*")]
             [(keyword (second (re-find #"rel=\"([^\"]+)\"" link)))
              (second (re-find #"<([^>]+)>" link))])))


(defn fetch-page
  "Generic wrapper around Javascript's fetch method with some error
   handling and custom header parsing.
  
   Returns a map containing:
   1. items:  a vector of items for this page (an items shape depends on 
              the provided endpoint)
   2. next:   URL of the next page — older entries (smaller ids), or nil
   3. prev:   URL of the previous page — newer entries (larger ids), or nil"
  [url & {:keys [wait method]
          :or   {wait 3000
                 method "GET"}}]
  (go
    (when (pos? wait)
      (<! (timeout wait)))
    (try
      (let [res           (<p! (js/fetch url #js {:headers #js {:Authorization (str "Bearer " (:token @config))}
                                                  :method  method}))
            content-type  (or (.get (.-headers res) "content-type") "")
            body          (<p! (.text res))]
        (cond
          (not (.-ok res))
          (do  (log-err "HTTP error:" res "\n" body) nil)

          (not (str/includes? content-type "json"))
          (do  (log-err "expected JSON got" (pr-str content-type) "for" url "\n" body) nil)

          :else
          (let [data  (js/JSON.parse body)
                arr   (if (array? data) data #js [data])
                links (some-> (.get (.-headers res) "link") parse-link)]
            {:items (js->clj arr :keywordize-keys true)
             :next (:next links)
             :prev (:prev links)})))
      (catch :default e
        (log-err "failed to fetch page:" e)
        nil))))


(defn get-self
  "Returns the `Account` map for the account associated with the 
   provided authorization token, or `nil.`"
  []
  (go
    (let [url     (str    (:instance @config) "/api/v1/accounts/verify_credentials")
          account (some-> (<! (fetch-page url))
                          :items
                          first)]
      account)))


(defn get-following
  "Returns a single page with \"items\" populated with `Account` maps 
   for a given account associated with the provided id that this account is following."
  [id & [query_params]]
  (go
    (let [params    (merge {:limit 80} query_params)
          qs        (str "?" (.toString (js/URLSearchParams. (clj->js params))))
          url       (str (:instance @config) "/api/v1/accounts/" id "/following" qs)
          page      (<! (fetch-page url))]
      page)))


(defn get-following-all
  "Returns a vector of all collected `Accounts` across pages the user
   with the associated account id is following."
  [id]
  (go-loop [following-page (<! (get-following id))
            accounts []]

    (let [accounts  (into accounts (:items following-page))
          max-id    (some-> (:next following-page)
                            js/URL.
                            .-searchParams
                            (.get "max_id"))]
      (if max-id
        (recur (<! (get-following id {:max_id max-id})) accounts)
        accounts))))


(defn unfollow
  "Unfollows the account associated with the provided id.
  
   Returns a `Relationship` map."
  [id]
  (go
    (let [url          (str (:instance @config) "/api/v1/accounts/" id "/unfollow")
          relationship (-> (<! (fetch-page url {:method "POST" :wait 500}))
                           :items
                           first)]
      relationship)))


(defn main
  [& _args]
  ;; eagerly evalute config
  @config
  (go
    (when-let [account (<! (get-self))]
      (let [id       (:id account)
            accounts (<! (get-following-all id))]
        (doseq [acc accounts]
          (<! (unfollow (:id acc))))))))

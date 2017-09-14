(ns tfl-bikes.core
  (:require [clj-http.client :as http]
            [clojure.string :as string]
            [hiccup.core :refer [html]]
            [yada.yada :as yada]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config

(def config {:api-url "https://api.tfl.gov.uk/bikepoint/"
             :creds   {:app-id  "2d3d3bf5"
                       :app-key "973a7bf6f5372437cd0667ad846b02e5"}
             :pos     {:lat 52
                       :lon -1}})

(defonce srv (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bikepoints

(defn get-with-params
  "Wrapper around clj-http/get encoding app query params.
   Returns a JSON response or an error map."
  ([creds path]
   (get-with-params creds path {}))
  ([{:keys [app-id app-key]} path params]
   (try
     (-> (http/get path {:as           :json
                         :query-params (merge
                                        {"app_id"  app-id
                                         "app_key" app-key}
                                        params)}))
     (catch Exception e
       (clojure.pprint/pprint (str "Error retrieving data from " path ". Error: " (.getMessage e)))
       {:error "An error occurred retrieving bike data."}))))

(defn- get-nb-bikes-for-bikepoint
  "Request and parse the response for the number
   of available bikes at a given bikepoint."
  [creds api-url bp]
  (let [bikes (->> (get-with-params creds (str api-url (:id bp)))
                   :body :additionalProperties
                   (filter #(= "NbBikes" (:key %)))
                   first
                   :value)]
    (assoc bp :bikes bikes)))

(defn- process-bikepoints
  "Based on a supplied latitude and longitude, calculate
   the absolute distance of each bikepoint and sort.
   Returns the 5 closest bike points."
  [{:keys [lat lon]} fields bps]
  (let [trimmed-bps (map #(select-keys % fields) bps)]
    (->> trimmed-bps
         (map (fn smallest-distance [bp]
                (let [abs-diff #(Math/abs (- (double %1) (double %2)))
                      lat-diff (abs-diff lat (:lat bp))
                      lon-diff (abs-diff lon (:lon bp))
                      square   #(* % %)]
                  (assoc bp :distance (Math/sqrt (+ (square lat-diff) (square lon-diff)))))))
         (sort-by :distance)
         (take 5)))) 

(defn- format-bikepoint [bp fields]
  [:tr (map #(do [:td (% bp)]) fields)])

(defn- search-bikepoints-content [fields bikepoints]
  (let [title "BikePoints"]
    (html
     [:head [:title title]]
     [:body
      [:h1 title]
      (if-let [error (:error bikepoints)]
        [:p error]
        [:div
         [:table
          [:tr (map #(do [:th (string/capitalize (name %))]) fields)]
          (map #(format-bikepoint % fields) bikepoints)]])])))

(defn search-bikepoints-resource [{:keys [creds api-url pos]}]
  (yada/resource
   {:id :index
    :methods
    {:get {:produces "text/html"
           :response (fn [ctx]
                       (let [query-fields   [:id :commonName :lat :lon :url]
                             display-fields (conj query-fields :distance :bikes)
                             bikepoints     (:body (get-with-params creds (str api-url "search") {"query" "waterloo"}))]
                         (->> bikepoints
                              (process-bikepoints pos query-fields)
                              (map #(get-nb-bikes-for-bikepoint creds api-url %))
                              (search-bikepoints-content display-fields))))}}}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic auth

(defn- restricted-content [ctx]
  (html
   [:head [:title "restricted content"]]
   [:body
    [:h1 (format "Welcome, %s."
                 (get-in ctx [:authentication "default" :user]))]
    [:p "Restricted resource"]
    [:pre (pr-str (get-in ctx [:authentication "default"]))]]))


(def basic-auth-resource
  (yada/resource
   {:id :basic-auth
    :methods
    {:get {:produces "text/html"
           :response (fn [ctx] (restricted-content ctx))}}
    
    :access-control
    {:scheme "Basic"
     :verify (fn [[user password]]
               ;; normally it's a good idea to check the password too
               (when (= user "juxt")
                 {:user user
                  :roles #{:user}}))
     :authorization {:methods {:get :user}}}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; index

(defn- index-content [ctx]
  (let [title "Yada and Bidi Bikepoints Example"]
    (html
     [:head [:title title]]
     [:body
      [:h1 title]
      [:div
       [:p
        [:a {:href "/auth"} "Basic Auth"]
        [:span " - username: juxt, no password"]]
       [:p
        [:a {:href "/search-bikepoints"} "Bike Points"]]]])))

(def index-resource
  (yada/resource
   {:id :index
    :methods
    {:get {:produces "text/html"
           :response (fn [ctx] (index-content ctx))}}}))


(defn routes [config]
  ["" [["/search-bikepoints" (search-bikepoints-resource config)]
       ["/auth"              #'basic-auth-resource]
       ["/"                  #'index-resource]
       [true                 (yada/as-resource nil)]]])


(defn server [config]
  (clojure.pprint/pprint "Server starting on 3000")
  (yada/listener
   (routes config)
   {:port 3000}))

(defn start
  "Starts a server on port 3000."
  []
  (reset! srv (server config)))

(defn stop []
  (swap! srv (fn [{:keys [close]}] (close))))

(defn restart []
  (stop)
  (start))


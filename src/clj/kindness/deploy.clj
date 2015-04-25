(ns clj.kindness.deploy
  (:require [amazonica.aws.cloudfront :as cloudfront]
            [amazonica.aws.s3 :as s3]
            [clojure.java.io :refer [file]]
            [environ.core :refer [env]])
  (:import [com.amazonaws.auth BasicAWSCredentials]
           [com.amazonaws.auth.profile
            ProfileCredentialsProvider]
           [com.amazonaws.services.cloudfront
            AmazonCloudFrontClient]
           [com.amazonaws.services.cloudfront.model
            Paths InvalidationBatch CreateInvalidationRequest]
           [com.amazonaws.services.s3 AmazonS3Client]
           [com.amazonaws.services.s3.model
            PutObjectRequest
            CannedAccessControlList]))

;; Requires credentials stored in ~/.aws/credentials
;; with a [kindness] section
(def aws-profile (get env :aws-profile "kindness"))
(def credentials {:profile aws-profile})
(def distribution-id (env :distribution-id))
(def bucket-name (env :aws-bucket))

(defn all-invalidations
  [distribution-id]
  (-> (cloudfront/list-invalidations
       credentials
       :distribution-id distribution-id)
      :invalidation-list
      :items))

(defn in-progress-invalidations
  [distribution-id]
  (filter #(= (:status %) "InProgress")
          (all-invalidations distribution-id)))

(defn upload-file
  [key upload-file]
  (let [creds (ProfileCredentialsProvider. aws-profile)
        s3client (AmazonS3Client. creds)
        putObj (PutObjectRequest. bucket-name
                                  key
                                  upload-file)]
    (.setCannedAcl putObj CannedAccessControlList/PublicRead)
    (.putObject s3client putObj)))

(defn create-invalidation
  [filename]
  (let [creds (ProfileCredentialsProvider. aws-profile)
        client (AmazonCloudFrontClient. creds)
        paths (.withItems (Paths.) filename)
        batch (InvalidationBatch. paths (java.util.UUID/randomUUID))
        invalidation (CreateInvalidationRequest. distribution-id batch)]
    (.createInvalidation client invalidation)))

(defn deploy-release
  []
  (let [key "js/compiled/out-adv/kindness.min.js"
        filename (str "/" key)
        file-to-upload (file (str "resources/public" filename))]
    (upload-file key file-to-upload)
    (create-invalidation filename)))

(defn -main
  []
  (deploy-release))

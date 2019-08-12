(ns pbtic.birthday.mail-tpl)

(defn body [{:keys [first-name]}]
  (format "Happy birthday, dear %s!" first-name))


(defn full [{:keys [email] :as employee}]
  [email, "Happy birthday!", (body employee)])

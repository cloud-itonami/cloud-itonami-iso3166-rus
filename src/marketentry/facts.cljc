(ns marketentry.facts "Russian Federation market-entry catalog.")
(def catalog
  {"RUS" {:name "Russian Federation"
          :owner-authority "Federal Treasury / EIS zakupki"
          :legal-basis "Federal Law 44-FZ / 223-FZ on procurement"
          :national-spec "EIS (zakupki.gov.ru) supplier registration + INN/OGRN"
          :provenance "https://zakupki.gov.ru/"
          :required-evidence ["INN/OGRN record"
                              "EIS registration record"
                              "EGRUL extract"
                              "Authorized-representative record"]
          :rep-owner-authority "contracting authorities / Federal Treasury"
          :rep-legal-basis "Russian legal entity (INN/OGRN) typically required for EIS participation"
          :rep-provenance "https://zakupki.gov.ru/"
          :corporate-number-owner-authority "FNS / FTS"
          :corporate-number-legal-basis "INN / OGRN"
          :corporate-number-provenance "https://www.nalog.gov.ru/"}
   "USA" {:name "United States" :owner-authority "GSA/SAM.gov" :legal-basis "FAR"
          :national-spec "SAM.gov" :provenance "https://sam.gov/"
          :required-evidence ["EIN record" "SAM.gov registration record" "State business registration record" "SAM UEI verification record"]}
   "CHN" {:name "China" :owner-authority "MOF procurement" :legal-basis "Government Procurement Law"
          :national-spec "China Government Procurement" :provenance "http://www.ccgp.gov.cn/"
          :required-evidence ["USCC record" "CCGP registration" "Business license" "Authorized-representative record"]}
   "DEU" {:name "Germany" :owner-authority "e-Vergabe" :legal-basis "GWB/VgV"
          :national-spec "e-Vergabe" :provenance "https://www.evergabe-online.de/"
          :required-evidence ["Handelsregister extract" "e-Vergabe registration record" "USt-IdNr record" "Authorized-representative record"]}})

(defn spec-basis [iso3] (get catalog iso3))
(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s) missing (remove catalog iso3s)]
     {:requested (count iso3s) :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note "R0 catalog seed"})))
(defn required-evidence-satisfied? [iso3 submitted]
  (when-let [{:keys [required-evidence]} (spec-basis iso3)]
    (= (count required-evidence) (count (filter (set submitted) required-evidence)))))
(defn evidence-checklist [iso3] (:required-evidence (spec-basis iso3) []))
(defn rep-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:rep-owner-authority sb)
      (select-keys sb [:rep-owner-authority :rep-legal-basis :rep-provenance]))))
(defn corporate-number-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:corporate-number-owner-authority sb)
      (select-keys sb [:corporate-number-owner-authority :corporate-number-legal-basis :corporate-number-provenance]))))

(ns culture.facts
  "Country-level regional-culture catalog for Russia (RUS) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.

  Two candidates were researched and dropped: borscht (the Wikipedia
  article states it originated in Ukraine, with UNESCO explicitly
  rejecting exclusive-ownership claims by any single country, so it was
  not honestly attributable to Russia specifically) and beluga caviar
  (the article emphasizes the beluga sturgeon's range spans five Caspian
  Sea states -- Iran, Azerbaijan, Kazakhstan, Russia, Turkmenistan -- with
  no exclusive Russian association, so it was not included either).")

(def catalog
  "iso3 -> vector of culture entries."
  {"RUS"
   [{:culture/id "rus.dish.pelmeni"
     :culture/name "Pelmeni"
     :culture/country "RUS"
     :culture/kind :dish
     :culture/summary "Dumplings of Russian cuisine that consist of a filling wrapped in thin, unleavened dough, considered a national dish of Russia."
     :culture/url "https://en.wikipedia.org/wiki/Pelmeni"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "rus.dish.blini"
     :culture/name "Blini"
     :culture/country "RUS"
     :culture/kind :dish
     :culture/summary "Russian pancakes, often made with a yeast-raised batter of buckwheat and/or wheat flour and milk, traditionally served with garnishes such as smetana, cottage cheese or caviar."
     :culture/url "https://en.wikipedia.org/wiki/Blini"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "rus.dish.shchi"
     :culture/name "Shchi"
     :culture/country "RUS"
     :culture/kind :dish
     :culture/summary "Traditional soup of Russia that became a staple food by the 10th century."
     :culture/url "https://en.wikipedia.org/wiki/Shchi"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "rus.beverage.kvass"
     :culture/name "Kvass"
     :culture/country "RUS"
     :culture/kind :beverage
     :culture/summary "Fermented, cereal-based, low-alcoholic beverage that originates from northeastern Europe and has become one of the symbols of East Slavic cuisine, remaining culturally significant in Russia."
     :culture/url "https://en.wikipedia.org/wiki/Kvass"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "rus.craft.matryoshka-doll"
     :culture/name "Matryoshka doll"
     :culture/country "RUS"
     :culture/kind :craft
     :culture/summary "The first Russian nested doll set was carved in the 1890s by craftsman Vasily Zvyozdochkin from a design by folk painter Sergey Malyutin, and the dolls continue to be mass-produced as a traditional handicraft to this day."
     :culture/url "https://en.wikipedia.org/wiki/Matryoshka_doll"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "rus.craft.khokhloma"
     :culture/name "Khokhloma"
     :culture/country "RUS"
     :culture/kind :craft
     :culture/summary "A style of Russian art traditionally painted on wooden household items, featuring curved linear motifs of flowers, berries and leaves, originating in the 17th-century village of Khokhloma in the Volga region."
     :culture/url "https://en.wikipedia.org/wiki/Khokhloma"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "rus.heritage.kremlin-and-red-square"
     :culture/name "Kremlin and Red Square"
     :culture/country "RUS"
     :culture/kind :heritage
     :culture/summary "The Kremlin and Red Square in Moscow were among the first Soviet patrimonies inscribed on the UNESCO World Heritage List, in 1990, under the official designation \"Kremlin and Red Square, Moscow\"."
     :culture/url "https://en.wikipedia.org/wiki/Kremlin_and_Red_Square"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-rus culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "RUS"))
                 " RUS entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))

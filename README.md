# prettymapr
Make beautiful maps based on OSM in R

Try to create a beautiful postcard.

```
center = tibble::tibble(53.54514883288921, 10.000039228206994, "HAMBURG")
osm_object <- get_osmdata(center, 1205*sqrt(2),1200)

color <- get_theme("macau", "Poppins")
scaling <- get_scaling("A4")

p <- prettymapr::plot_map(osm_object, color, scaling, T)
p
save_map(p, "C:/Users/user1/Documents/", osm_object, scaling, color, "A4_circle")
```

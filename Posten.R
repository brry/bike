# PORT Posten interaktive Karte
# Berry Boessenkool, Jul/Sept 2019, berry-b@gmx.de

library(leaflet) # leaflet, addTiles, addCircleMarkers
library(leaflet.extras) # addControlGPS, gpsOptions, activateGPS
library(berryFunctions) # seqPal, classify, popleaf
library(OSMscale) # github dev version! earthDist, pointsMap, projectPoints, posm

# Postenliste ----

p <- readxl::read_excel("PORT_Posten.xls")
p$Jahr <- as.numeric(substr(p$ID,1,nchar(p$ID)-2))
p$Zeile <- 1:nrow(p) # Fuer Routen Auswahl
p$popup <- popleaf(p, -(6:7))

leaflet(p) %>% addTiles() %>% addCircleMarkers(~Lon, ~Lat, popup=~popup)

# Fuer alle:
p_route <- p
p_route$Finden[1] <- 7 # 5 sollte ein dnkleres blau werden
p_route$col <- classify(p_route$Finden, col=seqPal, b=TRUE, breaks=NULL, reverse=TRUE)
leaflet(p_route) %>% addTiles() %>%
       addCircleMarkers(~Lon, ~Lat, popup=~popup,color=~col)

# Route ----

p_route <- p[c(263,371:400,1),] # Posten Fahrlaender See
p_route <- p[c(187,424,341,426,427,336,428,191,295,429,430,51,431,222,432,433,
               343,418,87,4,312,434:437,361,438,50,439,82,440:451,1),] # Posten Templiner See

# Farben fuer Kategorien  1 Photo    2 Rätsel     3 Bonus     Templiner See
#                               B                                       B           
gr <- c(1,1,1,2,2,1,2,1,1,3,2,1,3,1,3,2,3,3,3,3,3,2,1,1,2,1,2,2,1,1,3,1,3,2,1,1,1,1,2,1,1,1,1)
p_route$col <- c("black","red","orange")[gr]
rm(gr)


# Posten Tabelle ----

p_route$ID <- 1:nrow(p_route)
p_route$Schwere <- ceiling(rowMeans(p_route[,c("Erreichen","Finden")]))
p_route$popup <- popleaf(p_route, c("Titel","Erreichen","Finden","ID"))
colnames(p_route)[1] <- "Beschreibung"

write.table(p_route[,c("ID","Lat","Lon","Schwere","Beschreibung")], 
            file="Posten.txt", sep="\t", quote=FALSE, row.names=FALSE)

# Manuell in Excel kopieren und dort aendern, besonders Spalte "Schwere"


# Karte PDF ----

if(!file.exists("osmap.Rdata"))  {
  osmap14 <- pointsMap(Lat,Lon, data=p_route, zoom=14)
  save(osmap14, file="osmap.Rdata")
  }
load("osmap.Rdata")
ppr <- projectPoints(Lat,Lon, data=p_route, to=posm(), quiet=TRUE)

{
pdf("Templiner_Karte.pdf", width=8.27, height=11.96)
pointsMap(Lat,Lon, data=p_route, map=osmap14, quiet=TRUE, pch=1, cex=1.5, col=p_route$col)
text(ppr$x+70, ppr$y, p_route$ID,      cex=1,   adj=c(0,0), col=p_route$col)
textField(1450070,6868037, "12 km")
textField(1448250,6864745, "10 km")
textField(1443956,6864826, "5 km")
textField(osmap14$bbox$p1[1]+200, osmap14$bbox$p1[2]-200, "brry.github.io/bike (zoombar)", 
          adj=c(0,1))
dev.off()
}


# Karte interaktiv ----

{
map <- leaflet(p_route) %>% addTiles() %>%
       addCircleMarkers(~Lon, ~Lat, popup=~popup,color=~col) %>% #, stroke=F,radius=6)
       addControlGPS(options=gpsOptions(position="topleft", 
                     activate=TRUE, autoCenter=FALSE, maxZoom=60, setView=TRUE))
# exportieren:
htmlwidgets::saveWidget(map, "index.html", selfcontained=TRUE)
# HTML head fuer mobile Geraete:
# https://stackoverflow.com/questions/42702394/make-leaflet-map-mobile-responsive
map_h <- readLines("index.html")
map_h <- sub('<title>leaflet</title>', x=map_h,
 '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>\n<title>Bike Challenge</title>')
writeLines(map_h, "index.html") ; rm(map_h)
}
  







# ++ Hilfscode ++ ----


if(FALSE) { # Alles weitere beim Sourcen ignoriert 


# OSMtracker GPX Datei ----
# Fuer neue Stationen
visGPX::visGPX("2019-09-04_12-56-58.gpx", plot_static=FALSE, wp_column="name")
wp <- plotKML::readGPX("2019-09-04_12-56-58.gpx")$waypoints
clipr::write_clip(data.frame(wp$name,wp$ele, round(wp$lat,6),round(wp$lon,6)))


# Raetsel Templiner ----

r <- read.table(sep="\t", as.is=TRUE, text="Schafgraben
Spiel
Absteigen
Am Gaisberg 12a
Wentorfgraben
1942
weiß
Havel
Der älteste
Kieskutenberg
Steganlagen
")[,1]
cat(sapply(r, function(d) paste(rep("- ", nchar(d)), collapse="")), sep="\n")

txt <- "Segeln auf der Havel - zwischen Werder (Havel) und Brandenburg an der Havel rund 25 km ohne eine einzige Brücke oder Schleuse.
Verträumte und stille Nebengewässer für Kanuten - Unser Tipp: Die Wublitz bei Potsdam, "
cat(unlist(strsplit(gsub(" ", "", txt),"")), sep="\n")

# Map code ----

#addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me",
#       onClick=JS("function(btn, map){map.locate({setView:true,enableHighAccuracy: true}); }")))

#map <- activateGPS(map)

#<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />


# Karte mit Luftdistanzen um ungefaehre Routenlaenge zu pruefen ----

p_route$dist <- earthDist("Lat", "Lon", p_route, along=TRUE, quiet=TRUE)
p_route$distR <- round0(p_route$dist, digits=1, pre=1)
p_route$distC <- round0(cumsum(p_route$dist), digits=1, pre=1)
leaflet(p_route) %>% addTiles() %>% addCircleMarkers(~Lon, ~Lat, popup=~popup) %>% 
  addLabelOnlyMarkers(~Lon, ~Lat, label=~distC, labelOptions=labelOptions(noHide=T, textOnly=T)) %>% addPolylines(~Lon, ~Lat)


# Duplikate Postennamen----
dupli <- p[duplicated(p$Titel) | duplicated(p$Titel, fromLast=TRUE),]
for(tt in unique(dupli$Titel))
dupli[dupli$Titel==tt,"ddist"] <- OSMscale::maxEarthDist("Lat", "Lon", dupli[dupli$Titel==tt,], fun=min, each=FALSE)
leaflet(dupli[dupli$ddist<1.5,]) %>% addTiles() %>% addCircleMarkers(~Lon, ~Lat, popup=~popup)

p$col <- seqPal(100, b=T)[classify(p$Jahr)$index]

}

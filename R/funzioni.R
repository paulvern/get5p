library(httr)
library(xml2)
listlast<-function(month,year){
fullpath<-paste0("https://scihub.copernicus.eu/catalogueview/S5P/",year,"/",month,"/")
lista<-read_html(fullpath)
nodes<-trimws(html_text(html_nodes(lista, "a")))
return (nodes[6:length(nodes)])}
get5plist<-function(month,year,number){
 nodes<-listlast(month,year)
  leggo<-(nodes[as.numeric(number)+5])
  leggopath<-paste0(fullpath,leggo)
  leggoresult<-read.csv(leggopath)
return(leggoresult)
}
get5p<-function(month,year,number,id,fn)
  {
  leggo<-get5plist(month,year,number)
  p2<-paste0("https://s5phub.copernicus.eu/dhus/odata/v1/Products('",leggo$Id[id],"')/$value")
  getandsave<-GET(p2,authenticate("s5pguest", "s5pguest"))
  save(getandsave,file=fn)
}

get5p_latlon<-function(lat,lon,id=NULL)
{
  leggo<-paste0("https://s5phub.copernicus.eu/dhus/search?q=footprint:\"Intersects(",lat,",",lon,")\"")
  lista<-read_html(GET(leggo,authenticate("s5pguest", "s5pguest")))
  nodes<-trimws(html_text(html_nodes(lista, "id")))
  nodes2<-trimws(html_text(html_nodes(lista, "title")))
  nodi<-data.frame(name=nodes2[2:length(nodes2)],id=nodes[2:length(nodes)])
  if (!is.null(id)){
    leggo<-nodi$id[id]
    p2<-paste0("https://s5phub.copernicus.eu/dhus/odata/v1/Products('",leggo,"')/$value")
    getandsave<-GET(p2,authenticate("s5pguest", "s5pguest"))
    nomefile<-paste0(as.character(nodi$name[id]),".nc")
    save(getandsave,file=nomefile)
    nodi<-paste0("Salvato ",nomefile, " con successo")
  }
  return (nodi)
}

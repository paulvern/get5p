
#' List the latest files available in the Coperniucs 5p catalogue
#'
#' This function searches inside the current catalogue of archived products for the Platform Sentinel 5P (https://scihub.copernicus.eu/catalogueview/S5P/)
#' and returns a list of the archived .csv files of a specific month and year.
#' @param month Desired month in mm format (April would be "04", December would be "12")
#' @param year Desired year in yyyy format ("2020" not "20")
#' @keywords list
#' @export
#' @examples
#' listlast("04","2020")
listlast<-function(month,year){

fullpath<-paste0("https://scihub.copernicus.eu/catalogueview/S5P/",year,"/",month,"/")
lista<-xml2::read_html(fullpath)
nodes<-trimws(rvest::html_text(rvest::html_nodes(lista, "a")))
return (nodes[6:length(nodes)])}

#' Get a list of 5p products
#'
#' The functions works after listlast(month, year) function. After retrieving the list of available .csv files with listlast(),
#' the get5plist() functions allows to retrieve a list of the available products described inside a specific .csv file.
#' @param month Desired month in mm format (April would be "04", December would be "12")
#' @param year Desired year in yyyy format ("2020" not "20")
#' @param number Desired .csv file index (from 1 to the maximum number of files returned by listlast() function)
#' @keywords list
#' @export
#' @examples
#' get5plist("04","2020","3")
get5plist<-function(month,year,number){
 nodes<-listlast(month,year)
 fullpath<-paste0("https://scihub.copernicus.eu/catalogueview/S5P/",year,"/",month,"/")
  leggo<-(nodes[as.numeric(number)+5])
  leggopath<-paste0(fullpath,leggo)
  leggoresult<-read.csv(leggopath)
return(leggoresult)
}

#' Get a specific S5p product
#'
#' The functions works after get5plist() function, it retrives a specific S5P product and allows to save it
#' @param month Desired month in mm format (April would be "04", December would be "12")
#' @param year Desired year in yyyy format ("2020" not "20")
#' @param number Desired .csv file index (from 1 to the maximum number of files returned by listlast() function)
#' @param id Desired S5P file index (from 1 to the maximum number of files returned by get5plist() function)
#' @param fn Desired name of the file to be used when saving the product to the local working directory
#' @keywords list
#' @export
#' @examples
#' get5p("04","2020","3","1","lastday.ncf")
get5p<-function(month,year,number,id,fn)
  {
  leggo<-get5plist(month,year,number)
  p2<-paste0("https://s5pguest:s5pguest@s5phub.copernicus.eu/dhus/odata/v1/Products('",leggo$Id[id],"')/$value")
  download.file(p2,as.character(leggo$Name[id]))
}


#' Get the latest S5p products (lat long search)
#'
#' The function works by searching the latest 10 products available for a specific location (lat long).
#' It allows to download a specific product after identifying it.
#' @param lat Latitude (degrees) to search at
#' @param lon Longitude (degrees) to search at
#' @param id=NULL If the id parameter is omitted, the function returns a list of up to 10 S5p products available at the given coordinates,
#'  if an id is specified the functions downloads the product with the indicated id. If id=-1 then all the found products will be downloaded automatically.
#' @keywords list, download
#' @export
#' @examples
#' get5p_latlon("44","12")
#' get5p_latlon("44","12",1)
get5p_latlon<-function(lat,lon,id=NULL)
{
  leggo<-paste0("https://s5phub.copernicus.eu/dhus/search?q=footprint:\"Intersects(",lat,",",lon,")\"")
  lista<-xml2::read_html(httr::GET(leggo,httr::authenticate("s5pguest", "s5pguest")))
  sommario<-trimws(rvest::html_text(rvest::html_nodes(lista, "summary")))
  nodes<-trimws(rvest::html_text(rvest::html_nodes(lista, "id")))
  nodes2<-trimws(rvest::html_text(rvest::html_nodes(lista, "title")))
  nodi<-data.frame(desc=sommario,name=nodes2[2:length(nodes2)],id=nodes[2:length(nodes)])
  if (!is.null(id)){
    if(id!="-1"){
    leggo<-nodi$id[id]
    p2<-paste0("https://s5pguest:s5pguest@s5phub.copernicus.eu/dhus/odata/v1/Products('",leggo,"')/$value")
    nomefile<-paste0(as.character(nodi$name[id]),".nc")
    print(paste0("Downloading ",nomefile))
    download.file(p2,as.character(nodi$name[id]))

    print(paste0(nomefile, " successfully saved"))
  }
  if(id=="-1"){
    for(i in 1:length(nodi$id)){
      leggo<-nodi$id[i]
      print(paste0("Downloading file ",i,"/",length(nodi$id)))
      p2<-paste0("https://s5pguest:s5pguest@s5phub.copernicus.eu/dhus/odata/v1/Products('",leggo,"')/$value")
      nomefile<-paste0(as.character(nodi$name[i]),".nc")
      print(paste0("Downloading ",nomefile))
      download.file(p2,nomefile)
      print(paste0(nomefile, " successfully saved"))
    }}

  }
  return (nodi)
}
#' Get a S5p product
#'
#' This function allows to inspect a downloaded S5p file or plots it
#' @param fil Name of the file to inspect/plot
#' @param variable If the field is NULL then the function will return a list containing all the informations of the .nc file.
#' Normally the variables names are included as PRODUCT/variable. If the variable name is given as parameter then the function
#' will plot that variable and save the resulting plot as export.svg. Plotting the variable may require some time, depending on
#' the size of the downloaded file.
#' @keywords list, download, plot
#' @export
#' @examples
#' plot5p("S5P_OFFL_L2__AER_LH_20200527T115918_20200527T134048_13579_01_010302_20200529T045906.nc")
#' plot5p("S5P_OFFL_L2__AER_LH_20200527T115918_20200527T134048_13579_01_010302_20200529T045906.nc","aerosol_index_340_380")

plot5p<-function(fil,variable=NULL){
  nc<-ncdf4::nc_open(fil)
  lat<-ncdf4::ncvar_get(nc,"PRODUCT/latitude")
  if(!is.null(variable)){
lon<-ncdf4::ncvar_get(nc,"PRODUCT/longitude")
pro<-ncdf4::ncvar_get(nc,paste0("PRODUCT/",variable))
plottami<-data.frame(lat=as.vector(lat),lon=as.vector(lon),valore=as.vector(pro))
p<-ggplot2::ggplot(plottami, ggplot2::aes(y=lat, x=lon, fill=valore)) +
  ggplot2::geom_tile(width=1, height=1) +
  ggplot2::borders('world', xlim=range(plottami$lon), ylim=range(plottami$lat),
          colour='gray90', size=.2)
ggplot2::ggsave(file="export.png",p,width=10,height = 8)}
return(nc)
}

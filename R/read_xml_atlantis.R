# Helper function to create Xpath query and return
# argument from XML file
read_xml_atlantis <- function(xml_input, xpath_string){
  full_xpath <- paste("//Attribute[@AttributeName='",
                xpath_string,
                "']", sep="")
  tag_string <- xml2::xml_find_first(xml_input,
                full_xpath)
  return(as.character(tag_string))
}

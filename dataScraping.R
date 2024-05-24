# Install necessary packages
  # Wdman is used to install the selenium server and drivers for the web browsers.
  # Binman is used to check which drivers is installed on the hard drive.
  # Netstat is used solely for the free_port() function.
  # RSelenium is what is used to drive the browser and perform the scraping.

install.packages(c("wdman", "binman", "netstat", "RSelenium"))

# Install the selenium server and drivers.
wdman::selenium()

# This command can be used to check the location on the hard drive where the drivers
# are installed. This is necessary in case one has to manually install drivers.
wdman::selenium(retcommand = T, check = F)

# Because I'm using Google Chrome for the scraping process, I check the installed chromedrivers.
# Make sure that the version of Chrome that you are intending to use has matching drivers installed.
binman::list_versions("chromedriver")

# My setup is as follows:
  # Google Chrome 120.0.6099.200
  # Chromedriver 120.0.6099.109
  # Java JDK 21.0.1
  # Selenium Standalone Server 4.0.0-alpha-2
  # RSelenium 1.7.9

# And this is the tutorial that I used to set everything up:
# https://www.youtube.com/watch?v=GnpJujF9dBw&t=846s&pp=ygUPcnNlbGVuaXVtIHNhbWVy






# Begin.
library("RSelenium")
library("tidyverse")

# Basic tutorial on how to use RSelenium.
vignette("basics", package = "RSelenium")

# Launch the server and create a browser object in order to navigate the website.
driver <- rsDriver(browser = "chrome", port = netstat::free_port())
browser <- driver$client

# Navigate to the website.
browser$maxWindowSize()
browser$navigate("https://webbdiarium.mprt.se")

# Once I've reached the website, I fill out the search criteria by hand. This is
# easier, faster and more reliable. The search criteria that I use looks like this:
  # Search phrase: one of "svt", "p1", "p2", "p3", "p4, "kunskapskanalen", "barnkanalen", "svtb", "svt barn"
  # Decision date: from 2013-01-01 to 2023-01-01
  # Decision type: one of "Friande - grn" and "Fällande - grn"

# This is the function that I use to populate the data frames with metadata about the cases.
scrape <- function() {
  # The number of cases is displayed at the bottom of the page. We need this in order to determine
  # the length of the first for-loop. Here I save the number of cases as an integer.
  num_cases <- 
    browser$findElement(using = "xpath", value = "//*[@id='case-panel']/app-case/div/div/div/span")$getElementText() %>% 
    unlist() %>% 
    str_extract(., "\\d+") %>% 
    as.integer(.)
  
  # The number of pages is not displayed but we can calculate this using the number of cases.
  # Each page can contain up to ten cases.
  num_pages <- ceiling(num_cases/10)
  
  # An empty data frame to store the metadata and an empty vector to store case numbers
  # of cases without available files.
  df <- data.frame()
  dup_documents <- as.character()
  
  # Loop that collects metadata from all pages.
  for(page in 1:num_pages) {
    # For the second for-loop we need to know how many cases are visible on the current page.
    cases_on_page <- 
      browser$findElements(using = "xpath", 
                           value = "//*[@id='case-panel']/app-case/div/div/div/table/tbody//td[1]/button")
    
    # The second for-loop iterates through all cases on the current page. First, it clicks on a case
    # which opens up a window. Second, it collects data and stores it variables before binding it
    # to the data frame. Finally, it closes the window and moves on to the next case.
    for (i in 1:length(cases_on_page)) {
      cases_on_page[[i]]$clickElement()
      Sys.sleep(1) # This can be increased if errors occur.
      # Collect data.
      case <- 
        browser$findElement(
          using = "xpath", 
          value = "/html/body/ngb-modal-window/div/div/app-modal-popup/div[2]/app-case-details/div/div[1]/div[1]/div[3]")$getElementText() %>% 
        unlist()
      title <- 
        browser$findElement(
          using = "xpath", 
          value = "/html/body/ngb-modal-window/div/div/app-modal-popup/div[2]/app-case-details/div/div[1]/div[2]/div[3]")$getElementText() %>% 
        unlist()
      program <- 
        browser$findElement(
          using = "xpath", 
          value = "/html/body/ngb-modal-window/div/div/app-modal-popup/div[2]/app-case-details/div/div[1]/div[6]/div[3]")$getElementText() %>% 
        unlist()
      date <- 
        browser$findElement(
          using = "xpath", 
          value = "/html/body/ngb-modal-window/div/div/app-modal-popup/div[2]/app-case-details/div/div[1]/div[8]/div[3]")$getElementText() %>% 
        unlist()
      decision_date <- 
        browser$findElement(
          using = "xpath", 
          value = "/html/body/ngb-modal-window/div/div/app-modal-popup/div[2]/app-case-details/div/div[1]/div[9]/div[3]")$getElementText() %>% 
        unlist()
      decision <- 
        browser$findElement(
          using = "xpath", 
          value = "/html/body/ngb-modal-window/div/div/app-modal-popup/div[2]/app-case-details/div/div[1]/div[10]/div[3]")$getElementText() %>% 
        unlist()
      
      # Store data in data frame.
      df <- bind_rows(df, data.frame(case, title, program, date, decision_date, decision))
      
      
      document <- 
        browser$findElement(
          using = "xpath", 
          value = "/html/body/ngb-modal-window/div/div/app-modal-popup/div[2]/app-case-details/div/div[3]/button")
      
      # If there's an available file to download, initiate the process of clicking through the links
      # to find the download link and download the file. If there are two available files, save the case number in a vector.
      if (document$getElementText() %>% unlist() == "Documents (1)") {
        document$clickElement()
        Sys.sleep(1)
        browser$findElement(
          using = "xpath",
          value = "//*[contains(@id, '-header')]/h5/button")$clickElement()
        Sys.sleep(1)
        browser$findElement(
          using = "xpath",
          value = "/html/body/ngb-modal-window/div/div/app-modal-popup/div[2]/app-case-details/div/div[4]/div/div/div/div/ngb-accordion/div/div[2]/app-document-details/div/div[2]/div[1]/button")$clickElement()
        Sys.sleep(1)
        browser$findElement(
          using = "xpath",
          value = "//*[@id='collapseExample']/div/div/div/div/table/tbody/tr/td[2]/button")$clickElement()
      } 
      else if (document$getElementText() %>% unlist() == "Documents (2)") {
        dup_documents <- c(dup_documents, case)
      }
      
      # Close the window.
      browser$findElement(using = "xpath", 
                          value = "/html/body/ngb-modal-window/div/div/app-modal-popup/div[3]/button")$clickElement()
    }
    
    # Identify the element that navigates to the next page and click it.
    browser$findElement(using = "link text", value = "»")$clickElement()
    
    # Wait for the next page to load. Can be increased if connection speed is slow.
    Sys.sleep(1)
  }
  
  # Return the data frame containing the metadata about the cases.
  return(list(df, dup_documents))
}

# Here I use the function to create the lists for the different channels
# and outcomes. Before you run these commands you have to fill out the search
# criteria manually as mentioned above.
svt_notguilty <- scrape()
svt_notguilty2 <- scrape()
svt_guilty <- scrape()
p1_notguilty <- scrape()
p1_guilty <- scrape()
p2_notguilty <- scrape()
p2_guilty <- scrape() # No cases found.
p3_notguilty <- scrape()
p3_guilty <- scrape()
p4_notguilty <- scrape()
p4_guilty <- scrape()
kunskap_notguilty <- scrape()
kunskap_guilty <- scrape()
barn_notguilty <- scrape()
barn_guilty <- scrape()
svtb_notguilty <- scrape()
svtb_guilty <- scrape() 
svtbarn_notguilty <- scrape()
svtbarn_guilty <- scrape() # No cases found.

# Final data frame with all data.
final_df <- bind_rows(svt_notguilty[1],
                      svt_notguilty2[1],
                      svt_guilty[1],
                      p1_notguilty[1],
                      p1_guilty[1],
                      p2_notguilty[1],
                      p3_notguilty[1],
                      p3_guilty[1],
                      p4_notguilty[1],
                      p4_guilty[1],
                      kunskap_notguilty[1],
                      kunskap_guilty[1],
                      barn_notguilty[1],
                      barn_guilty[1],
                      svtb_notguilty[1],
                      svtb_guilty[1],
                      svtbarn_notguilty[1]) %>%
  mutate(across(everything(), ~ na_if(., "")),
         decision = gsub("Friande – grn", "Acquittal", decision),
         decision = gsub("Fällande – grn", "Conviction", decision),
         decision = factor(decision),
         channel = case_when(
           grepl("Kunskapskanalen", title) ~ "Kunskapskanalen",
           grepl("Barnkanalen", title) ~ "SVT Barn",
           grepl("SVTB", title) ~ "SVT Barn",
           grepl("SVT Barn", title) ~ "SVT Barn",
           grepl("UR", title) ~ "UR",
           grepl("SVT", title) ~ "SVT",
           grepl("P1", title) ~ "SR",
           grepl("P2", title) ~ "SR",
           grepl("P3", title) ~ "SR",
           grepl("P4", title) ~ "SR",
           grepl("SR", title) ~ "SR",
           TRUE ~ NA_character_),
         channel = factor(channel),
         date = str_extract(date, "\\d{4}-\\d{2}-\\d{2}")) %>% 
  select(case, channel, program, date, title, decision_date, decision) %>% 
  arrange(case)

dup_documents <- c(p1_notguilty[2] %>% unlist(), p1_guilty[2] %>% unlist(),
                   p2_notguilty[2] %>% unlist(),
                   p3_notguilty[2] %>% unlist(), p3_guilty[2] %>% unlist(),
                   p4_notguilty[2] %>% unlist(), p4_guilty[2] %>% unlist(),
                   kunskap_notguilty[2] %>% unlist(), kunskap_guilty[2] %>% unlist(),
                   barn_notguilty[2] %>% unlist(), barn_guilty[2] %>% unlist(),
                   svtb_notguilty[2] %>% unlist(), svtb_guilty[2] %>% unlist(),
                   svtbarn_notguilty[2] %>% unlist()) %>% 
  as.data.frame() %>% 
  mutate(across(everything(), ~ na_if(., ""))) %>% 
  na.omit() %>% 
  distinct(.)

#write_csv(svt_notguilty, "svt_notguilty.csv")
#write_csv(svt_guilty, "svt_guilty.csv")
#write_csv(p1_notguilty, "p1_notguilty.csv")
#write_csv(p1_guilty, "p1_guilty.csv")
#write_csv(p2_notguilty, "p2_notguilty.csv")
#write_csv(p3_notguilty, "p3_notguilty.csv")
#write_csv(p3_guilty, "p3_guilty.csv")
#write_csv(p4_notguilty, "p4_notguilty.csv")
#write_csv(p4_guilty, "p4_guilty.csv")
#write_csv(final_df, "final_df.csv")
#write_csv(dup_documents, "dup_documents.csv")





# Close the browser and server.
driver$client$close()
driver$server$stop()

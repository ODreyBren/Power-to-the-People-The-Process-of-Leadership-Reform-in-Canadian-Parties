# Purpose
r code for Chapter 5 of the doctoral dissertation "Beyond the Ballot: Reviewing Canadian Political Parties' Leadership Selection Rules" completed at Université Laval and Université libre de Bruxelles (Cevipol). This chapter analysed the content of semi-structured interview transcripts of political party actors having witnessed or contributed to a leadership selectorate change. 

## Notes on data
The party documents were collected between July 2022 and December 2023. To protect the anonymity of our respondents, the only data published is the quantitative data taken from the internal party reports. There are no text segments in this dataset. Please contact the author for more information on the text dataset and the party documents.

This is the code used to create the various Figures in Chapter 5.

## Research Questions
** COMING SOON

# Code
```R
#set path
setwd("~/_rCodesData/chapter3InterviewsReform")
```

## Load the necessary package to wrangle the data 
```
library(tidyverse)
```

## Load the data
```
data <- read_csv("docCodeMatrixBrowser_MAXQDA.csv")
```


## Transform the data for nicer Figures
```
#Create a custom order for the Reform documents by year
custom_order <- c("LPC Proposed Constitution 2006",
                  "LPC Delegate Reform 2006",
                  "LPC Every Voter Counts 2008",
                  "LPC Advacing Change 2009",
                  "LPC Report of the Change Comission 2011",
                  "LPC Building a Modern Liberal Party 2011",
                  "LPC Supporter By-Law 2012",
                  "BC Liberal Reform 2010",
                  "OLP Reform 2023",
                  "PLQ Plan 2013 Couillard",
                  "NB Reform annoucement Website",
                  "NB Reform consultation report")

# Convert Reform.document to a factor with the custom order
data$`Reform Document` <- factor(data$`Reform Document`, levels = custom_order)
```

## Plot the Inherent Conditions 
File Name `inherentByDoc.pdf`

```
# Transform the data to a long format
data_long <- data %>%
  pivot_longer(cols = -`Reform Document`, names_to = "Category", values_to = "Count")

# Transform the data to a long format and filter the categories
inherent_categories <- c("Inherent General", 
                         "Inherent Internal Party Logistics", 
                         "Inherent Party Goals", 
                         "Inherent Technology")

data_long <- data %>%
  pivot_longer(cols = -`Reform Document`, names_to = "Category", values_to = "Count") %>%
  filter(Category %in% inherent_categories)


# Rename the anomalous categories
data_long <- data_long %>%
  mutate(Category = recode(Category,
                           "Inherent General" = "General",
                           "Inherent Internal Party Logistics" = "Internal Logistics",
                           "Inherent Party Goals" = "Party Goals",
                           "Inherent Technology" = "Technology"))

  # Create the ggplot
            ggplot(data_long, aes(x = `Reform Document`, y = Count, fill = Category)) +
              geom_bar(stat = "identity", position = "stack" , na.rm = TRUE) +
              coord_flip() +
              theme_void() +
              labs(title = " ",
                   x = " ",
                   y = " ",
                   fill = "Category") +
              scale_x_discrete(limits = rev(custom_order)) +
              guides(fill = guide_legend(reverse = TRUE, title = NULL)) + # Reorder the legend so colors apear in same order in figure as in lengend
              scale_fill_grey() +
              theme_bw() +
              theme( # Facet_wrap labels
                strip.text.x = element_text(
                  size = 16, color = "black", face = "bold"
                ),
                strip.text.y = element_text(
                  size = 12, color = "black", face = "bold"
                ),
                strip.background=element_rect(fill=NA, color=NA),
                # Legend labels
                legend.title = element_text(color = "black", size = 16, face = "bold"),
                legend.text = element_text(color = "black", size = 14, face = "bold"), 
                legend.position="bottom",
                axis.text.x = element_text(color="black", 
                                           size=12),
                axis.text.y = element_text(face="bold", color="black", 
                                           size=14)
              )
```


## Plot the Anomalous outcomes
File Name: `anomalousByDoc.pdf`
```
    # Transform the data to a long format and filter the categories
  anomalous_categories <- c("Anomalous Outcomes", 
                             "Anomalous Outcomes Membership", 
                             "Anomalous Outcomes Leadership race", 
                             "Anomalous Outcomes Election results,
                             Anomalous Outcomes Laws")
    

    
    data_long <- data %>%
      pivot_longer(cols = -`Reform Document`, names_to = "Category", values_to = "Count") %>%
      filter(Category %in% anomalous_categories) 
    
    
    # Rename the inherent categories
    data_long <- data_long %>%
      mutate(Category = recode(Category,
                               "Anomalous Outcomes" = "General", 
                             "Anomalous Outcomes Membership" = "Membership", 
                             "Anomalous Outcomes Leadership race" = "Leadership", 
                             "Anomalous Outcomes Election results" = "Election Results",
                             "Anomalous Outcomes Laws" = "Laws"))
    

    
    # Create the ggplot
    ggplot(data_long, aes(x = `Reform Document`, y = Count, fill = Category)) +
      geom_bar(stat = "identity", position = "stack", na.rm = TRUE) +
      coord_flip() +
      theme_void() +
      labs(title = " ",
           x = " ",
           y = " ",
           fill = "Category") +
      scale_x_discrete(limits = rev(custom_order)) +
      scale_y_continuous(breaks = seq(0, max(data_long$Count), by = 1)) + # Specify y-axis intervals
      guides(fill = guide_legend(reverse = TRUE, title = NULL)) + # Reorder the legend so colors apear in same order in figure as in lengend
      scale_fill_grey() +
      theme_bw() +
      theme( # Facet_wrap labels
        strip.text.x = element_text(
          size = 16, color = "black", face = "bold"
        ),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"
        ),
        strip.background=element_rect(fill=NA, color=NA),
        # Legend labels
        legend.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14, face = "bold"), 
        legend.position="bottom",
        axis.text.x = element_text(color="black", 
                                   size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=14)
      )

```

## Plot the Act Contignent

File Name: `AContingencyByDoc`


```
 # Transform the data to a long format and filter the categories
    Acontingent_categories <- c(
                              "Act Contingent - General", 
                             "Act Contingent - Other parties",
                             "Act Contingent - IPD-Seeking"
                             )
    
    

    data_long <- data %>%
      pivot_longer(cols = -`Reform Document`, names_to = "Category", values_to = "Count") %>%
      filter(Category %in% Acontingent_categories) 
    
    
    # Rename the Act Contingent categories
    data_long <- data_long %>%
      mutate(Category = recode(Category,
                               "Act Contingent - General" = "General", 
                               "Act Contingent - Other parties" = "AC Other Parties",
                               "Act Contingent - IPD-Seeking" = "AC IPD-Seeking"))
    
    
    # Create the ggplot
    ggplot(data_long, aes(x = `Reform Document`, y = Count, fill = Category)) +
      geom_bar(stat = "identity", position = "stack", na.rm = TRUE) +
      coord_flip() +
      theme_void() +
      labs(title = " ",
           x = " ",
           y = " ",
           fill = "Category") +
      scale_x_discrete(limits = rev(custom_order)) +
      scale_y_continuous(breaks = seq(0, max(data_long$Count), by = 1)) + # Specify y-axis intervals
      guides(fill = guide_legend(reverse = TRUE, title = NULL)) + # Reorder the legend so colors apear in same order in figure as in lengend
      scale_fill_grey() +
      theme_bw() +
      theme( # Facet_wrap labels
        strip.text.x = element_text(
          size = 16, color = "black", face = "bold"
        ),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"
        ),
        strip.background=element_rect(fill=NA, color=NA),
        # Legend labels
        legend.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14, face = "bold"), 
        legend.position="bottom",
        axis.text.x = element_text(color="black", 
                                   size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=14)
      )
```
## Plot the Outcome Contingent

File Name: `OContingencyByDoc.pdf`
```
  # Transform the data to a long format and filter the categories
    Ocontingent_categories <- c("Outcome Contingent", 
                                "Outcome Contingent - Office-seeking",
                                "Outcome Contingent - Vote-Seeking", 
                                "Outcome Contingent - Policy-Seeking"
    )
    
    
    
    data_long <- data %>%
      pivot_longer(cols = -`Reform Document`, names_to = "Category", values_to = "Count") %>%
      filter(Category %in% Ocontingent_categories) 
    
    
    # Rename the Outcome Contingent categories
    data_long <- data_long %>%
      mutate(Category = recode(Category,
                               "Outcome Contingent" = "General", 
                               "Outcome Contingent - Office-seeking" = "OC Office-Seeking",
                               "Outcome Contingent - Vote-Seeking" = "OC Vote-Seeking", 
                               "Outcome Contingent - Policy-Seeking" = "OC Policy-Seeking"))
    
    
    # Create the ggplot
    ggplot(data_long, aes(x = `Reform Document`, y = Count, fill = Category)) +
      geom_bar(stat = "identity", position = "stack", na.rm = TRUE) +
      coord_flip() +
      theme_void() +
      labs(title = " ",
           x = " ",
           y = " ",
           fill = "Category") +
      scale_x_discrete(limits = rev(custom_order)) +
      scale_y_continuous(breaks = seq(0, max(data_long$Count), by = 1)) + # Specify y-axis intervals
      guides(fill = guide_legend(reverse = TRUE, title = NULL)) + # Reorder the legend so colors apear in same order in figure as in lengend
      scale_fill_grey() +
      theme_bw() +
      theme( # Facet_wrap labels
        strip.text.x = element_text(
          size = 16, color = "black", face = "bold"
        ),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"
        ),
        strip.background=element_rect(fill=NA, color=NA),
        # Legend labels
        legend.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14, face = "bold"), 
        legend.position="bottom",
        axis.text.x = element_text(color="black", 
                                   size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=14)
      )

```

## Plot the Reform Enactment
File Name: `REByDoc.pdf`

```
 # Transform the data to a long format and filter the categories
    re_categories <- c("Reform Enactment - Procedure",
                      "Reform Enactment  Legislation",
                      "Reform Enactment - Referendum"
    )
    
    
    
    data_long <- data %>%
      pivot_longer(cols = -`Reform Document`, names_to = "Category", values_to = "Count") %>%
      filter(Category %in% re_categories) 
    
    # Rename the Reform Enactment categories
    data_long <- data_long %>%
      mutate(Category = recode(Category,
                               "Reform Enactment - Procedure" = "Procedure",
                               "Reform Enactment  Legislation" = "Legislation",
                               "Reform Enactment - Referendum" = "Referendum"))
    

    
    # Create the ggplot
    ggplot(data_long, aes(x = `Reform Document`, y = Count, fill = Category)) +
      geom_bar(stat = "identity", position = "stack", na.rm = TRUE) +
      coord_flip() +
      theme_void() +
      labs(title = " ",
           x = " ",
           y = " ",
           fill = "Category") +
      scale_x_discrete(limits = rev(custom_order)) +
      scale_y_continuous(breaks = seq(0, max(data_long$Count), by = 1)) + # Specify y-axis intervals
      guides(fill = guide_legend(reverse = TRUE, title = NULL)) + # Reorder the legend so colors apear in same order in figure as in legend
      scale_fill_grey() +
      theme_bw() +
      theme( # Facet_wrap labels
        strip.text.x = element_text(
          size = 16, color = "black", face = "bold"
        ),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"
        ),
        strip.background=element_rect(fill=NA, color=NA),
        # Legend labels
        legend.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14, face = "bold"), 
        legend.position="bottom",
        axis.text.x = element_text(color="black", 
                                   size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=14)
      )


```
    
    
```

# Load necessary libraries
library(dplyr)
library(tidyr)
library(purrr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(httr) 
library(stringr)

# Set your USDA API Key
api_key <- "hlWoTaS171xsl2tDcrJnxRlUuGTGVVUssFHc3qjd"

# Initialize database connection
db <- dbConnect(SQLite(), "usda_food_database.sqlite")

# Create table for food data if it does not exist
dbExecute(db, "CREATE TABLE IF NOT EXISTS food_data (
                  fdcId INTEGER PRIMARY KEY,
                  description TEXT
               )")

# Create table for nutrient data if it does not exist
dbExecute(db, "CREATE TABLE IF NOT EXISTS nutrient_data (
                  fdcId INTEGER,
                  nutrientName TEXT,
                  amount REAL,
                  unitName TEXT,
                  FOREIGN KEY (fdcId) REFERENCES nutrient_data(fdcId)
               )")

# Initialize variables
page_size <- 200  # Max allowed by API
page_number <- 1

repeat {
  # Construct API URL with pagination
  url <- paste0("https://api.nal.usda.gov/fdc/v1/foods/list?",
                "pageSize=", page_size,
                "&pageNumber=", page_number,
                "&api_key=", api_key)
  
  # Fetch data from API
  response <- GET(url)
  
  # Stop if API request fails
  if (status_code(response) != 200) break
  
  # Convert JSON response to list
  data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  # Stop when no more data is available
  if (length(data) == 0) break
  
  # Extract food descriptions
  food_info <- data %>% select(fdcId, description)
  
  # Insert food data into the database
  dbWriteTable(db, "food_data", food_info, append = TRUE, row.names = FALSE)
  
  # Extract all nutrients for each food item and insert into the database
  nutrient_data <- map2_df(data$fdcId, data$foodNutrients, function(fdc_id, nutrients) {
    if (is.null(nutrients) || length(nutrients) == 0) return(NULL)
    
    # Convert nutrients list to dataframe
    nutrients_df <- bind_rows(nutrients) %>%
      select(name, amount, unitName) %>%
      mutate(fdcId = fdc_id) %>%
      rename(nutrientName = name)  # Rename to match SQL table
    
    return(nutrients_df)
  })
  
  if (nrow(nutrient_data) > 0) {
    dbWriteTable(db, "nutrient_data", nutrient_data, append = TRUE, row.names = FALSE)
  }
  
  print(paste("Page", page_number, "inserted into database."))
  
  # Increment page number
  page_number <- page_number + 1
  
  # Pause to avoid API rate limits
  Sys.sleep(1)
}
# ✅ Close the database connection
dbDisconnect(db)
# Connect to the database
db <- dbConnect(SQLite(), "usda_food_database.sqlite")
# ✅ Verify by listing all tables in the database
print(dbListTables(db))
food_data <- dbReadTable(db, "food_data")
nutrient_data <- dbReadTable(db, "nutrient_data")


# Optionally, merge the two tables to get a combined final_dataset:
final_final_dataset <- nutrient_data %>%
  left_join(food_data, by = "fdcId") %>%
  select(description, nutrientName, amount) %>%
  pivot_wider(names_from = nutrientName, values_from = amount)



final_final_dataset<-final_final_dataset %>%
  select(description, Energy, Protein,`Calcium, Ca`,`Iron, Fe`,`Carbohydrate, by difference`,Caffeine, Water, `Total Sugars`,`Total lipid (fat)`,`Magnesium, Mg`, `Potassium, K`,`Sodium, Na`, `Zinc, Zn`)
# Convert list columns to strings, except for "description"
final_final_dataset <- final_final_dataset %>%
  mutate(across(where(is.list), ~ map_chr(., ~ toString(unlist(.))))) %>%  # Convert lists to strings
  mutate(across(where(is.character) & -description, ~ as.numeric(str_extract(., "^[0-9.]+"))))  # Convert characters to numeric



# Write `final_final_dataset` to the same SQLite database as a new table
dbWriteTable(db, "food_dataset", final_final_dataset, overwrite = TRUE, row.names = FALSE)

# Verify by listing all tables in the database
print(dbListTables(db))

# lose the database connection
dbDisconnect(db)


print("All food data successfully stored in SQLite database!")

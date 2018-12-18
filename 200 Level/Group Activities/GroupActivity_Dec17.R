

raw_vector <- c(
  "OrderID",
  "DateAdded",
  "State",
  "Zip",
  "Email",
  "Address",
  "Address2",
  "MarketID",
  "ProgramName",
  "ChannelName",
  "PhoneCarrier_Twilio",
  "MundoSales",
  "TV.UltimateVideoSales",
  "TV.ExtremeVideoSales",
  "TV.PreferredVideoSales",
  "TV.CustomVideoSales",
  "TV.LocalVideoSales",
  "QuantumTV.Sales",
  "FiosVideoSales",
  "FiosDataSales",
  "FiosDataHighPlanSales",
  "DownSpeed",
  "UpSpeed",
  "FiosVoiceSales",
  "RevenueOrders",
  "SinglePlayNE.Sales",
  "DoublePlayNE.Sales",
  "TriplePlayNonExistingSales",
  "FiosNonExistingSales",
  "TotalPostSales",
  "InstalledRev",
  "EmployeeID",
  "TalkTIme",
  "Browser",
  "BrowserVersion",
  "CallPage",
  "City",
  "Region",
  "Country",
  "DeviceType",
  "Device",
  "LandingPage",
  "InternetSpeed",
  "ZipCode_GeoIP",
  "ISP",
  "Referrer",
  "Domain",
  "AdGroup",
  "Campaign",
  "Keyword",
  "KeywordMatchType"
)

library(tidyverse)

remove_periods <- gsub("[.]", "_", raw_vector)
add_unscore <- gsub("([a-z0-9])([A-Z0-9])", "\\1_\\2", remove_periods)


lower_vector <- tolower(add_unscore)
lower_vector






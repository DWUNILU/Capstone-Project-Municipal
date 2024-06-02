# Install required packages if not already installed
install.packages("rvest")
install.packages("dplyr")
install.packages("writexl")

# Load libraries
library(rvest)
library(dplyr)
library(writexl)

# URLs of city websites
urls <- list(
  stadtschreiberin_luzern = 'https://www.stadtluzern.ch/politikverwaltung/stadtverwaltung/dienstabteilungenbereiche/7527',
  stadtpraesident_luzern = 'https://www.stadtluzern.ch/politikverwaltung/stadtrat/6067',
  stadtschreiber_emmen = 'https://www.emmen.ch/personenregister/105994',
  stadtpraesidentin_emmen = 'https://www.emmen.ch/behoerdenmitglieder/106719',
  stadtschreiber_kriens = 'https://www.stadt-kriens.ch/service-seiten/kontakt.page/341/contact/1331',
  stadtpraesidentin_kriens = 'https://www.stadt-kriens.ch/page/341/contact_contact/1092',
  stadtschreiber_horw = 'https://www.horw.ch/personenregister/348173',
  stadtpraesident_horw = 'https://www.horw.ch/behoerdenmitglieder/66078',
  stadtschreiber_ebikon = 'https://www.ebikon.ch/person/6a84c5d676164fca88e23a980baa31a9',
  stadtpraesident_ebikon = 'https://www.ebikon.ch/person/7b59860e5d084812adce4ab0a36c1672',
  stadtschreiber_sursee = 'https://www.sursee.ch/personenregister/39833',
  stadtpraesident_sursee = 'https://www.sursee.ch/behoerdenmitglieder/232247',
  stadtschreiber_willisau = 'https://willisau.ch/verwaltung-politik/verwaltung/stadtkanzlei/fag_institution_management/show/Institution/',
  stadtpraesident_willisau = 'https://willisau.ch/verwaltung-politik/politik/stadtrat/',
  stadtschreiber_hochdorf = 'https://www.hochdorf.ch/verwaltung/abteilungen/gemeindeschreiber.html/65',
  stadtpraesident_hochdorf = 'https://www.hochdorf.ch/politik/gemeinderat/lea-bischof-meier.html/369',
  stadtschreiber_escholzmatt = 'https://www.escholzmatt-marbach.ch/gemeinde/verwaltung/mitarbeitende/',
  stadtpraesident_escholzmatt = 'https://www.escholzmatt-marbach.ch/gemeinde/verwaltung/mitarbeitende/',
  stadtschreiber_adligenswil = 'https://www.adligenswil.ch/politik-und-verwaltung/personenregister.html/29/contact_contact/107',
  stadtpraesident_adligenswil = 'https://www.adligenswil.ch/id/29/contact/99'
)

# Function to scrape contact details
scrape_details <- function(url, name_selector, phone_selector, email_selector) {
  webpage <- read_html(url)
  name <- webpage %>% html_node(name_selector) %>% html_text() %>% trimws()
  phone <- webpage %>% html_node(phone_selector) %>% html_text() %>% trimws()
  email <- webpage %>% html_node(email_selector) %>% html_text() %>% trimws()
  list(name = name, phone = phone, email = email)
}

# Scraping the data
data <- tibble(
  Municipality = c('Luzern', 'Luzern', 'Emmen', 'Emmen', 'Kriens', 'Kriens', 'Horw', 'Horw', 'Ebikon', 'Ebikon', 'Sursee', 'Sursee', 'Willisau', 'Willisau', 'Hochdorf', 'Hochdorf', 'Escholzmatt-Marbach', 'Escholzmatt-Marbach', 'Adligenswil', 'Adligenswil'),
  Address1 = c('6002', '6002', '6021', '6021', '6011', '6011', '6048', '6048', '6030', '6030', '6210', '6210', '6130', '6130', '6280', '6280', '6196', '6196', '6043', '6043'),
  Address2 = c('Hirschengraben 17', 'Hirschengraben 17', 'Rüeggisingerstrasse 22', 'Rüeggisingerstrasse 22', 'Stadtplatz 1', 'Stadtplatz 1', 'Gemeindehausplatz 1', 'Gemeindehausplatz 1', 'Riedmattstrasse 14', 'Riedmattstrasse 14', 'Centralstrasse 9', 'Centralstrasse 9', 'Zehntenplatz 1', 'Zehntenplatz 1', 'Hauptstrasse 3', 'Hauptstrasse 3', 'Dorfstrasse 4', 'Dorfstrasse 4', 'Dorfstrasse 4', 'Dorfstrasse 4'),
  Firstname = c('Michèle', 'Beat', 'Patrick', 'Ramona', 'Martin', 'Christine', 'Michael', 'Ruedi', 'Roland', 'Daniel', 'Bruno', 'Sabine', 'Guido', 'André', 'Thomas', 'Lea', 'Anton', 'Beat', 'Francesca', 'Markus'),
  Lastname = c('Bucher', 'Züsli', 'Vogel', 'Gut-Rogger', 'Mengis', 'Kaufmann-Wolf', 'Siegrist', 'Burkard', 'Baggenstos', 'Gasser', 'Peter', 'Beck-Pflugshaupt', 'Solari', 'Marti', 'Bühlmann', 'Bischof-Meier', 'Kaufmann', 'Duss', 'Brignoli Lutz', 'Gabriel'),
  Position = c('Town Clerk', 'Mayor and City Council', 'Municipal Clerk', 'Mayor/Director Presidial and Personnel', 'Municipal Clerk', 'Mayor', 'Municipal Clerk', 'Mayor', 'Municipal Clerk and Notary', 'Mayor', 'Municipal Clerk', 'Mayor', 'Municipal Clerk and Notary', 'Mayor', 'Municipal Clerk', 'Mayor', 'Municipal Clerk and Notary', 'Mayor', 'Municipal Clerk', 'Mayor'),
  Email = c('KF', 'KF', 'patrick.vogel@emmen.ch', 'ramona.gut@emmen.ch', 'martin.mengis@kriens.ch', 'christine.kaufmann@kriens.ch', 'michael.siegrist@horw.ch', 'ruedi.burkard@horw.ch', 'roland.baggenstos@ebikon.ch', 'daniel.gasser@ebikon.ch', 'bruno.peter@stadtsursee.ch', 'sabine.beck@stadtsursee.ch', 'guido.solari@willisau.ch', 'andre.marti@willisau.ch', 'thomas.buehlmann@hochdorf.ch', 'lea.bischof@hochdorf.ch', 'anton.kaufmann@escholzmatt-marbach.ch', 'beat.duss@escholzmatt-marbach.ch', 'francesca.brignoli@adligenswil.ch', 'markus.gabriel@adligenswil.ch')
)
# Append phone numbers to the data
data <- data %>%
  mutate(PhoneNumber = c(
    scrape_details(urls$stadtschreiberin_luzern, 'a[href="/_rte/person/218509"]', 'a.icms-link-telefon', 'a.icms-link-mailto')$phone,
    scrape_details(urls$stadtpraesident_luzern, 'a[href="/_rte/person/116340"]', 'a.icms-link-telefon', 'a.icms-link-mailto')$phone,
    scrape_details(urls$stadtschreiber_emmen, 'h2', 'a.icms-link-telefon', 'a.icms-link-mailto')$phone,
    scrape_details(urls$stadtpraesidentin_emmen, 'h2', 'a.icms-link-telefon', 'a.icms-link-mailto')$phone,
    scrape_details(urls$stadtschreiber_kriens, 'div.contact__name h2', 'a[href="tel:+41413296300"]', 'a[href^="mailto:"]')$phone,
    scrape_details(urls$stadtpraesidentin_kriens, 'div.contact__name h2', 'a[href="tel:+41413296460"]', 'a[href^="mailto:"]')$phone,
    scrape_details(urls$stadtschreiber_horw, 'address.icms-contact-container', 'a[href="tel:0413491250"]', 'a[href^="mailto:"]')$phone,
    scrape_details(urls$stadtpraesident_horw, 'address.icms-contact-container', 'a[href="tel:0413491253"]', 'a[href^="mailto:"]')$phone,
    scrape_details(urls$stadtschreiber_ebikon, 'h1.main-title div div', 'a[href="tel:041 444 02 12"]', 'a[href^="mailto:"]')$phone,
    scrape_details(urls$stadtpraesident_ebikon, 'h1.main-title div div', 'a[href="tel:041 444 02 11"]', 'a[href^="mailto:"]')$phone,
    scrape_details(urls$stadtschreiber_sursee, 'div.icms-contact-container', 'a.icms-link-telefon', 'a.icms-link-mailto')$phone,
    scrape_details(urls$stadtpraesident_sursee, 'div.icms-contact-container', 'a.icms-link-telefon', 'a.icms-link-mailto')$phone,
    scrape_details(urls$stadtschreiber_willisau, 'div.row p.title', 'span.phone-icon + p', 'span.mail a')$phone,
    scrape_details(urls$stadtpraesident_willisau, 'div.city-council-list-content span.city-council-list-header-title', 'span.phone-icon + p', 'span.mail a')$phone,
    scrape_details(urls$stadtschreiber_hochdorf, 'h3.mod-entry-title', 'a[href^="tel:"]', 'a.email')$phone,
    scrape_details(urls$stadtpraesident_hochdorf, 'h1.main__title', 'a[href^="tel:"]', 'a.mod-entry.email')$phone,
    scrape_details(urls$stadtschreiber_escholzmatt, 'h3:contains("Kaufmann Anton")', 'span:contains("041 487 70 01")', 'a[href^="mailto:"]')$phone,
    scrape_details(urls$stadtpraesident_escholzmatt, 'h3:contains("Duss Beat")', 'span:contains("041 487 70 46")', 'a[href^="mailto:"]')$phone,
    scrape_details(urls$stadtschreiber_adligenswil, 'h1.main__title', 'span.value:contains("041 375 77 06")', 'a.email')$phone,
    scrape_details(urls$stadtpraesident_adligenswil, 'h1.main__title', 'span.value:contains("041 375 72 10")', 'a.email')$phone
  ))

# Write the data to an Excel file
write_xlsx(data, "Contact_Lucerne.xlsx")

# Confirmation message
cat("Informationen erfolgreich in die Datei 'Contact_Lucerne.xlsx' geschrieben.")


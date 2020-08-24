# Set local path of chromedriver
chrome_path = '/Library/chromedriver'

# Set list of book URLs to be scraped
# (All URLs should begin with 'https://www.goodreads.com/book/show/')
book_urls = ['https://www.goodreads.com/book/show/42201629', 'https://www.goodreads.com/book/show/43521657',
'https://www.goodreads.com/book/show/37794149','https://www.goodreads.com/book/show/36510722',
'https://www.goodreads.com/book/show/42036538','https://www.goodreads.com/book/show/43401925','https://www.goodreads.com/book/show/53128611',
'https://www.goodreads.com/book/show/36546128', 'https://www.goodreads.com/book/show/43352954','https://www.goodreads.com/book/show/40939044',
'https://www.goodreads.com/book/show/42201962', 'https://www.goodreads.com/book/show/45454304']

# Set output directory (include final forward slash!)
output_dir = 'data/raw/'
# Set output name ('.csv' will be automatically appended)
output_name = 'reviews_Nebula__Novel_Novella'

# Set amount of time to sleep between browser actions (4 seconds is recommended)
zzz = 4